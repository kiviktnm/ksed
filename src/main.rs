mod config;
mod data;
mod eval;
mod layout;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::{self, File},
    io::{BufReader, Write},
    path::PathBuf,
    process::ExitCode,
    str::SplitWhitespace,
};

use config::*;
use data::*;
use layout::*;
use rustyline::{error::ReadlineError, DefaultEditor};
use serde::{Deserialize, Serialize};

use colored::Colorize;

use crate::eval::Evaluator;

const MAX_HISTORY_LENGTH: usize = 20;

#[derive(Debug)]
enum Error {
    InvalidLayerTextReprLength,
    InvalidLayerTextReprCharacter(char),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidLayerTextReprLength => {
                write!(f, "Invalid layer text representation length.")
            }
            Error::InvalidLayerTextReprCharacter(c) => write!(
                f,
                "Invalid character '{}' for layer text representation.",
                c
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Press {
    Right(Finger),
    Left(Finger),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Finger {
    Pinky,
    Ring,
    Middle,
    Index,
}

type Key = (usize, usize, usize);

fn key_to_press(key: Key) -> Option<Press> {
    HAND_ASSIGNMENTS
        .get(key.0)
        .and_then(|layer| layer.get(key.1))
        .and_then(|column| column.get(key.2))
        .copied()
}

#[derive(Debug, Serialize, Deserialize)]
struct AppState {
    main_layout: Layout,
    secondary_layout: Layout,
    layout_history: Vec<Layout>,
    datasets: HashMap<String, CharDataset>,
    verified_bad_layouts: HashSet<Layout>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            main_layout: Layout::dvorak(),
            secondary_layout: Layout::qwerty(),
            layout_history: Default::default(),
            datasets: Default::default(),
            verified_bad_layouts: Default::default(),
        }
    }
}

impl AppState {
    fn state_filename() -> PathBuf {
        PathBuf::from("ksed.bin")
    }

    fn restore_state() -> Result<AppState, Box<dyn std::error::Error>> {
        let filename = Self::state_filename();

        if !filename.try_exists()? {
            eprintln!("No previous state found.");
            return Ok(AppState::default());
        }

        let file = File::open(filename)?;
        let reader = BufReader::new(file);

        Ok(bincode::deserialize_from(reader)?)
    }

    fn save_state(&self) -> Result<(), Box<dyn std::error::Error>> {
        fs::write(Self::state_filename(), bincode::serialize(self)?)?;

        Ok(())
    }

    fn append_to_history(&mut self) {
        self.layout_history.push(self.main_layout.clone());

        if self.layout_history.len() > MAX_HISTORY_LENGTH {
            self.layout_history.remove(0);
        }
    }

    fn run_app(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let mut rl = DefaultEditor::new()?;

        loop {
            let line = rl.readline("> ");

            match line {
                Ok(cmd) => {
                    rl.add_history_entry(&cmd)?;
                    if self.handle_cmd_should_exit(&cmd) {
                        break;
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    break;
                }
                Err(e) => return Err(Box::new(e)),
            }
        }

        Ok(())
    }

    fn handle_cmd_should_exit(&mut self, cmd: &str) -> bool {
        let mut args = cmd.split_whitespace();

        if let Some(cmd) = args.next() {
            match cmd {
                "exit" | "quit" | "q" => return true,
                "print" | "p" => self.print(args),
                "copy" => self.copy(args),
                "swap" | "s" => self.swap(args),
                "switch" => self.switch(),
                "set" => self.set(args),
                "restore" => self.restore(args),
                "read" => self.read(args),
                "save" => self.save(),
                "chars" => self.print_items(args, CharDataset::sorted_chars, Self::print_chars),
                "all_chars" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_chars,
                    Self::print_chars,
                ),
                "bigrams" => {
                    self.print_items(args, CharDataset::sorted_bigrams, Self::print_bigrams)
                }
                "all_bigrams" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_bigrams,
                    Self::print_bigrams,
                ),
                "trigrams" => {
                    self.print_items(args, CharDataset::sorted_trigrams, Self::print_trigrams)
                }
                "all_trigrams" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_trigrams,
                    Self::print_trigrams,
                ),
                "vowels" => {
                    self.print_items(args, CharDataset::sorted_vowel_bigrams, Self::print_bigrams)
                }
                "all_vowels" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_vowel_bigrams,
                    Self::print_bigrams,
                ),
                "consonants" => self.print_items(
                    args,
                    CharDataset::sorted_consonant_bigrams,
                    Self::print_bigrams,
                ),
                "all_consonants" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_consonant_bigrams,
                    Self::print_bigrams,
                ),
                "specials" => {
                    self.print_items(args, CharDataset::sorted_special_chars, Self::print_chars)
                }
                "chargroups" => {
                    self.print_items(args, CharDataset::sorted_chargroups, Self::print_chargroups)
                }
                "all_chargroups" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_chargroups,
                    Self::print_chargroups,
                ),
                "rare_chargroups" => {
                    let get_sorted_chargroups_reversed = |dataset: &mut CharDataset| {
                        let mut items = dataset.sorted_chargroups();
                        items.reverse();
                        items
                    };

                    self.print_items(args, get_sorted_chargroups_reversed, Self::print_chargroups);
                }
                "rare_all_chargroups" => {
                    let get_sorted_chargroups_reversed = |datasets: Vec<&CharDataset>| {
                        let mut items = CharDataset::combined_sorted_chargroups(datasets);
                        items.reverse();
                        items
                    };

                    self.print_combined_items(
                        args,
                        get_sorted_chargroups_reversed,
                        Self::print_chargroups,
                    );
                }
                "rare_chargroups_with" => self.print_rare_chargroups_with(args),
                "words" => self.print_items(args, CharDataset::sorted_words, Self::print_words),
                "all_words" => self.print_combined_items(
                    args,
                    CharDataset::combined_sorted_words,
                    Self::print_words,
                ),
                "char_stats" => {
                    self.print_stats(args, CharDataset::sorted_chars, Layout::evaluate_char)
                }
                "bigram_stats" => {
                    self.print_stats(args, CharDataset::sorted_bigrams, Layout::evaluate_bigram)
                }
                "trigram_stats" => {
                    self.print_stats(args, CharDataset::sorted_trigrams, Layout::evaluate_trigram)
                }
                "word_stats" => {
                    self.print_word_stats(args);
                }
                "best" => self.best(args),
                "best_2" => self.best_uncompromising(args),
                "datasets" => self.print_datasets(),
                "length" => {
                    println!("Bad layouts found: {}", self.verified_bad_layouts.len());
                }
                "word_count" => {
                    for (name, dataset) in &self.datasets {
                        println!("{}: {}", name, dataset.unique_word_count());
                    }
                }
                "delete" => self.delete_dataset(args),
                "history" => self.history(),
                "improve" => {
                    if self.improve(args).is_none() {
                        eprintln!("Invalid improve command.");
                        eprintln!("usage: improve <branches> <trigram/bigram count> <word count>");
                    }
                }
                "slow_improve_loop" => {
                    if self.slow_improve(args).is_none() {
                        eprintln!("Invalid improve command.");
                        eprintln!(
                            "usage: slow_improve <branches> <trigram/bigram count> <word count>"
                        );
                    }
                }
                "alt_improve" => {
                    if self.alt_improve(args).is_none() {
                        eprintln!("Invalid alt_improve command.");
                        eprintln!("usage: alt_improve <initial_depth> <depth> <loops> <trigram/bigram count> <word count>");
                    }
                }
                "alt_improve_until" => {
                    if self.alt_improve_until(args).is_none() {
                        eprintln!("Invalid alt_improve_until command.");
                        eprintln!("usage: alt_improve_until <initial_depth> <depth> <loops> <trigram/bigram count> <word count> <max rounds>");
                    }
                }
                "improve_until" => {
                    if self.improve_until(args).is_none() {
                        eprintln!("Invalid improve_until command.");
                        eprintln!("usage: improve_until <branches> <trigram/bigram count> <word count> <max rounds>");
                    }
                }
                "version" => println!("29"),
                "help" => println!("Look at the source code. Good Luck!! :D"),
                unknown => eprintln!("Unknown command {unknown}"),
            }
        }

        false
    }

    fn save(&mut self) {
        if let Err(e) = self.save_state() {
            eprintln!("Saving failed.");
            eprintln!("{e}");
        }
    }

    fn delete_dataset(&mut self, mut args: SplitWhitespace) {
        if let Some(ds_name) = args.next() {
            if self.datasets.remove(ds_name).is_some() {
                println!("Dataset removed.")
            } else {
                eprintln!("No dataset with the given name found.")
            }
        } else {
            eprintln!("Invalid delete command.");
            eprintln!("Include a name of the dataset to delete.");
        }
    }

    fn improve(&mut self, mut args: SplitWhitespace) -> Option<()> {
        let branches = args.next().and_then(|arg| arg.parse::<u8>().ok())?;
        let n = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let k = args.next().and_then(|arg| arg.parse::<usize>().ok())?;

        self.append_to_history();

        let datasets = self.datasets.values().collect();
        let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

        println!("Before:");
        println!("{}", self.main_layout);
        self.main_layout = evaluator.improve_layout(self.main_layout.clone(), branches);

        println!();
        println!("After:");
        println!("{}", self.main_layout);

        Some(())
    }

    fn slow_improve(&mut self, mut args: SplitWhitespace) -> Option<()> {
        let branches = args.next().and_then(|arg| arg.parse::<u8>().ok())?;
        let n = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let k = args.next().and_then(|arg| arg.parse::<usize>().ok())?;

        loop {
            let new_layout;
            {
                println!(
                    "{}: Stating a new round.",
                    chrono::Local::now().time().format("%H:%M")
                );
                let datasets = self.datasets.values().collect();
                let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);
                new_layout =
                    evaluator.single_thread_improve_layout(self.main_layout.clone(), branches);
            }

            if self.main_layout != new_layout {
                println!(
                    "{}: Improvements found.",
                    chrono::Local::now().time().format("%H:%M")
                );
                println!("{new_layout}");
                self.append_to_history();
                self.main_layout = new_layout;
                if let Err(e) = self.save_state() {
                    eprintln!("Failed to save state: {e}");
                }
            }
        }
    }

    fn alt_improve(&mut self, mut args: SplitWhitespace) -> Option<()> {
        let initial_depth = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let depth = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let loops = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let n = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let k = args.next().and_then(|arg| arg.parse::<usize>().ok())?;

        self.append_to_history();

        let datasets = self.datasets.values().collect();
        let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

        println!("Before:");
        println!("{}", self.main_layout);
        println!();
        self.main_layout =
            evaluator.alt_improve_layout(self.main_layout.clone(), loops, depth, initial_depth);
        println!();
        println!("After:");
        println!("{}", self.main_layout);

        Some(())
    }

    fn alt_improve_until(&mut self, mut args: SplitWhitespace) -> Option<()> {
        let initial_depth = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let depth = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let loops = args.next().and_then(|arg| arg.parse::<u16>().ok())?;
        let n = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let k = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let max_rounds = args.next().and_then(|arg| arg.parse::<usize>().ok())?;

        self.append_to_history();

        let datasets = self.datasets.values().collect();
        let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

        println!("Before:");
        println!("{}", self.main_layout);
        println!();

        let mut best_result = self.main_layout.clone();
        let mut round_counter = 1;

        loop {
            print!("Round: {}/{}.", round_counter, max_rounds);
            let _ = std::io::stdout().flush();

            let new_result =
                evaluator.alt_improve_layout(best_result.clone(), loops, depth, initial_depth);

            if evaluator.is_first_layout_better(&new_result, &best_result)
                && round_counter <= max_rounds
                && new_result != best_result
            {
                best_result = new_result;
            } else {
                break;
            }

            round_counter += 1;
        }

        self.main_layout = best_result;

        println!();
        println!("After:");
        println!("{}", self.main_layout);
        Some(())
    }

    fn improve_until(&mut self, mut args: SplitWhitespace) -> Option<()> {
        let branches = args.next().and_then(|arg| arg.parse::<u8>().ok())?;
        let n = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let k = args.next().and_then(|arg| arg.parse::<usize>().ok())?;
        let max_rounds = args.next().and_then(|arg| arg.parse::<usize>().ok())?;

        self.append_to_history();

        let datasets = self.datasets.values().collect();
        let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

        println!("Before:");
        println!("{}", self.main_layout);
        println!();

        let mut best_result = self.main_layout.clone();
        let mut round_counter = 1;

        loop {
            print!("Round: {}/{}.", round_counter, max_rounds);
            let _ = std::io::stdout().flush();

            round_counter += 1;
            let new_result = evaluator.improve_layout(best_result.clone(), branches);

            if evaluator.is_first_layout_better(&new_result, &best_result)
                && round_counter <= max_rounds
                && new_result != best_result
            {
                best_result = new_result;
            } else {
                break;
            }
        }

        self.main_layout = best_result;

        println!();
        println!("After:");
        println!("{}", self.main_layout);

        Some(())
    }

    fn history(&self) {
        for (index, layout) in self.layout_history.iter().enumerate() {
            println!("{}", index);
            println!("{}", layout);
            println!();
        }
    }

    fn best(&mut self, mut args: SplitWhitespace) {
        println!("Warning: best will not evaluate key placement directly.");
        if let Some(n) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
            if let Some(k) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
                let datasets = self.datasets.values().collect();
                let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

                let mut best_layout_name = String::from("Main");
                let mut best_layout = &self.main_layout;

                if evaluator.is_first_layout_better(&self.secondary_layout, best_layout)
                    && self.secondary_layout != *best_layout
                {
                    best_layout_name = String::from("Secondary");
                    best_layout = &self.secondary_layout;
                }

                let dvorak = Layout::dvorak();
                if evaluator.is_first_layout_better(&Layout::dvorak(), best_layout) {
                    best_layout_name = String::from("DVORAK");
                    best_layout = &dvorak;
                }

                for (index, layout) in self.layout_history.iter().enumerate() {
                    if evaluator.is_first_layout_better(layout, best_layout)
                        && layout != best_layout
                    {
                        best_layout_name = index.to_string();
                        best_layout = &layout;
                    }
                }

                println!("Best layout is '{}'.", best_layout_name);
                println!("{}", best_layout);
            } else {
                eprintln!("Invalid best command.");
                eprintln!("usage: best <trigram/bigram count> <word count>");
            }
        } else {
            eprintln!("Invalid best command.");
            eprintln!("usage: best <trigram/bigram count> <word count>");
        }
    }

    fn best_uncompromising(&mut self, mut args: SplitWhitespace) {
        println!("Warning: best_2 will not evaluate key placement directly.");
        if let Some(n) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
            if let Some(k) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
                let datasets = self.datasets.values().collect();
                let evaluator = Evaluator::new(&datasets, n, k, &mut self.verified_bad_layouts);

                let mut best_layout_name = String::from("Main");
                let mut best_layout = &self.main_layout;

                if evaluator
                    .is_first_layout_better_uncompromising(&self.secondary_layout, best_layout)
                    && self.secondary_layout != *best_layout
                {
                    best_layout_name = String::from("Secondary");
                    best_layout = &self.secondary_layout;
                }

                let dvorak = Layout::dvorak();
                if evaluator.is_first_layout_better_uncompromising(&Layout::dvorak(), best_layout) {
                    best_layout_name = String::from("DVORAK");
                    best_layout = &dvorak;
                }

                for (index, layout) in self.layout_history.iter().enumerate() {
                    if evaluator.is_first_layout_better_uncompromising(layout, best_layout)
                        && layout != best_layout
                    {
                        best_layout_name = index.to_string();
                        best_layout = &layout;
                    }
                }

                println!("Uncompromisingly best layout is '{}'.", best_layout_name);
                println!("{}", best_layout);
            } else {
                eprintln!("Invalid best command.");
                eprintln!("usage: best_2 <trigram/bigram count> <word count>");
            }
        } else {
            eprintln!("Invalid best command.");
            eprintln!("usage: best_2 <trigram/bigram count> <word count>");
        }
    }

    fn set(&mut self, mut args: SplitWhitespace) {
        let layer1_txt = args.next();
        let layer2_txt = args.next();

        if layer1_txt.is_none() || layer2_txt.is_none() {
            eprintln!("Invalid set command.");
            eprintln!("Include layer 1 and 2 text representations.");
            return;
        }

        let new_layout_result = Layout::new(layer1_txt.unwrap(), layer2_txt.unwrap());

        if let Err(e) = new_layout_result {
            eprintln!("Creating a new layout failed.");
            eprintln!("Ensure that the layer text representations are correct.");
            eprintln!("{e}");
            return;
        }

        self.append_to_history();

        self.main_layout = new_layout_result.unwrap();
    }

    fn print_rare_chargroups_with(&self, mut args: SplitWhitespace) {
        let chars = args.next();

        if chars.is_none() {
            eprintln!("Invalid command.");
            eprintln!("Include at least 1 character.")
        }

        let mut chars = chars.unwrap().chars();

        let c1 = chars.next().unwrap_or('_');
        let c2 = chars.next();
        let c3 = chars.next();

        let get_sorted_chargroups_reversed = |datasets: Vec<&CharDataset>| {
            let mut items: Vec<_> = CharDataset::combined_sorted_chargroups(datasets)
                .into_iter()
                .filter(|cgr| cgr.item.contains(c1))
                .filter(|cgr| c2.is_none() || cgr.item.contains(c2.unwrap()))
                .filter(|cgr| c3.is_none() || cgr.item.contains(c3.unwrap()))
                .collect();
            items.reverse();
            items
        };

        self.print_combined_items(args, get_sorted_chargroups_reversed, Self::print_chargroups);
    }

    fn print_word_stats(&mut self, mut args: SplitWhitespace) {
        if let Some(k) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
            for (dataset_name, dataset) in &mut self.datasets {
                println!("{}", dataset_name.bold());
                println!();

                let words = dataset.sorted_words();
                let total_word_count = dataset.word_count();

                println!("{}", "Main layout:".underline());
                let stats =
                    self.main_layout
                        .calculate_word_stats(&words, total_word_count as f64, k);
                println!("{}", stats);
                println!();

                println!("{}", "Secondary layout:".underline());
                let stats =
                    self.secondary_layout
                        .calculate_word_stats(&words, total_word_count as f64, k);
                println!("{}", stats);
                println!();

                println!("{}", "QWERTY:".underline());
                let stats =
                    Layout::qwerty().calculate_word_stats(&words, total_word_count as f64, k);
                println!("{}", stats);
                println!();

                println!("{}", "DVORAK:".underline());
                let stats =
                    Layout::dvorak().calculate_word_stats(&words, total_word_count as f64, k);
                println!("{}", stats);
                println!();
            }
        } else {
            eprintln!("Invalid word_stats command.");
            eprintln!("usage: word_stats <word count>");
        }
    }

    fn print_stats<T>(
        &mut self,
        mut args: SplitWhitespace,
        get_items: impl Fn(&mut CharDataset) -> Vec<WithOccurance<T>>,
        evaluate_item: impl Fn(&Layout, &T) -> ScoreCategory,
    ) {
        if let Some(n) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
            for (dataset_name, dataset) in &mut self.datasets {
                println!("{}", dataset_name.bold());

                let items = get_items(dataset);

                fn print_scores_items<T>(
                    items: &[WithOccurance<T>],
                    n: usize,
                    layout: &Layout,
                    evaluate_item: impl Fn(&Layout, &T) -> ScoreCategory,
                    total_occurances: &mut f64,
                ) {
                    let n = n.min(items.len());

                    let mut scores = HashMap::new();
                    for c in &items[..n] {
                        let score = evaluate_item(layout, &c.item);
                        *scores.entry(score).or_insert(0) += 1;
                        *total_occurances += c.occ();
                    }
                    let mut scores_vec: Vec<_> = scores.iter().collect();
                    scores_vec.sort_by_key(|(score, _)| *score);
                    scores_vec.reverse();

                    for (score, count) in scores_vec {
                        println!("\t{} : {}", score, count);
                    }
                }

                let mut total_occurances = 0.0;

                println!("{}", "\tMain layout:".underline());
                print_scores_items(
                    &items,
                    n,
                    &self.main_layout,
                    &evaluate_item,
                    &mut total_occurances,
                );
                println!();

                println!("{}", "\tSecondary layout:".underline());
                print_scores_items(
                    &items,
                    n,
                    &self.secondary_layout,
                    &evaluate_item,
                    &mut total_occurances,
                );
                println!();

                println!("{}", "\tQWERTY:".underline());
                print_scores_items(
                    &items,
                    n,
                    &Layout::qwerty(),
                    &evaluate_item,
                    &mut total_occurances,
                );
                println!();

                total_occurances = 0.0;

                println!("{}", "\tDVORAK:".underline());
                print_scores_items(
                    &items,
                    n,
                    &Layout::dvorak(),
                    &evaluate_item,
                    &mut total_occurances,
                );
                println!();
                println!("\tCoverage (%): {:.4}", total_occurances);
                println!();
            }
        } else {
            eprintln!("Invalid stats command.");
            eprintln!("Include a valid number of elements to show.");
        }
    }

    fn print_datasets(&self) {
        for dataset_name in self.datasets.keys() {
            println!("{dataset_name}");
        }
    }

    fn print_combined_items<T>(
        &self,
        mut args: SplitWhitespace,
        get_items: impl Fn(Vec<&CharDataset>) -> Vec<T>,
        print_items: impl Fn(&Self, usize, &[T]),
    ) {
        if let Some(n) = args.next().and_then(|arg| arg.parse::<usize>().ok()) {
            let items = get_items(self.datasets.values().collect());
            print_items(self, n, &items);
        } else {
            eprintln!("Invalid all_* command.");
            eprintln!("Include a valid number of elements to show.");
        }
    }

    fn print_items<T>(
        &mut self,
        mut args: SplitWhitespace,
        get_items: impl Fn(&mut CharDataset) -> Vec<T>,
        print_items: impl Fn(&Self, usize, &[T]),
    ) {
        let dataset_name = args.next();
        let n = args.next();

        if dataset_name.is_none() || n.is_none() {
            eprintln!("Invalid command.");
            eprintln!("Include a dataset and a number of elements to show.");
            return;
        }

        let dataset_name = dataset_name.unwrap();
        let n = n.unwrap();

        if let Ok(n) = n.parse::<usize>() {
            if let Some(dataset) = self.datasets.get_mut(dataset_name) {
                let items = get_items(dataset);
                print_items(self, n, &items);
            } else {
                eprintln!("Unknown dataset '{dataset_name}'.");
            }
        } else {
            eprintln!("Cannot parse '{n}' to a number.");
        }
    }

    fn print_chargroups(&self, n: usize, chargroups: &[WithOccurance<CharGroup>]) {
        let n = n.min(chargroups.len());
        println!("{}", "Chars\t\tOcc %".bold());
        for c in &chargroups[..n] {
            println!("{}\t\t{:.4}", c.item, c.occ(),);
        }
    }

    fn print_words(&self, n: usize, words: &[WithOccurance<String>]) {
        let n = n.min(words.len());
        println!("{}", "Word\t\tOcc %".bold());
        for c in &words[..n] {
            println!("{}\t\t{:.4}", c.item, c.occ(),);
        }
    }

    fn print_trigrams(&self, n: usize, trigrams: &[WithOccurance<Trigram>]) {
        let n = n.min(trigrams.len());
        println!(
            "{}",
            "Trigram\t\tOcc %\t\tMain     \t\tSecondary\t\tQWERTY   \t\tDVORAK   ".bold()
        );
        for c in &trigrams[..n] {
            println!(
                "{}{}{}\t\t{:.4}\t\t{}\t\t{}\t\t{}\t\t{}",
                c.item.0,
                c.item.1,
                c.item.2,
                c.occ(),
                self.main_layout.evaluate_trigram(&c.item),
                self.secondary_layout.evaluate_trigram(&c.item),
                Layout::qwerty().evaluate_trigram(&c.item),
                Layout::dvorak().evaluate_trigram(&c.item)
            );
        }
    }

    fn print_bigrams(&self, n: usize, bigrams: &[WithOccurance<Bigram>]) {
        let n = n.min(bigrams.len());
        println!(
            "{}",
            "Bigram\t\tOcc %\t\tMain     \t\tSecondary\t\tQWERTY   \t\tDVORAK   ".bold()
        );
        for c in &bigrams[..n] {
            println!(
                "{}{}\t\t{:.4}\t\t{}\t\t{}\t\t{}\t\t{}",
                c.item.0,
                c.item.1,
                c.occ(),
                self.main_layout.evaluate_bigram(&c.item),
                self.secondary_layout.evaluate_bigram(&c.item),
                Layout::qwerty().evaluate_bigram(&c.item),
                Layout::dvorak().evaluate_bigram(&c.item)
            );
        }
    }

    fn print_chars(&self, n: usize, chars: &[WithOccurance<char>]) {
        let n = n.min(chars.len());
        println!(
            "{}",
            "Char\t\tOcc %\t\tMain     \t\tSecondary\t\tQWERTY   \t\tDVORAK   ".bold()
        );
        for c in &chars[..n] {
            println!(
                "{}\t\t{:.4}\t\t{}\t\t{}\t\t{}\t\t{}",
                c.item,
                c.occ(),
                self.main_layout.evaluate_char(&c.item),
                self.secondary_layout.evaluate_char(&c.item),
                Layout::qwerty().evaluate_char(&c.item),
                Layout::dvorak().evaluate_char(&c.item)
            );
        }
    }

    fn read(&mut self, mut args: SplitWhitespace) {
        let dataset_name = args.next();
        let filename = args.next();

        if dataset_name.is_none() || filename.is_none() {
            eprintln!("Invalid read command.");
            eprintln!("Include a dataset and file to read.");
            return;
        }

        let dataset_name = dataset_name.unwrap();
        let filename = filename.unwrap();

        let dataset = self
            .datasets
            .entry(dataset_name.to_string())
            .or_insert(CharDataset::create());

        if let Err(err) = dataset.parse_file(filename) {
            eprintln!("Failed to read file: {err}.")
        }
    }

    fn restore(&mut self, mut args: SplitWhitespace) {
        if let Some(index) = args.next().and_then(|s| s.parse::<usize>().ok()) {
            if let Some(layout) = self.layout_history.get(index) {
                let layout = layout.clone();

                self.append_to_history();

                self.main_layout = layout;
            } else {
                eprintln!(
                    "No layout in history with the given index '{index}'. History length is {}.",
                    self.layout_history.len()
                )
            }
        } else {
            eprintln!("Invalid index.");
        }
    }

    fn swap(&mut self, mut args: SplitWhitespace) {
        let s1 = args.next();
        let s2 = args.next();

        let c1 = s1.and_then(|s| s.chars().next());
        let c2 = s2.and_then(|s| s.chars().next());

        if c1.is_none() || c2.is_none() {
            eprintln!("Invalid swap command.");
            eprintln!("Include 2 keys to swap.");
            return;
        }

        let c1 = c1.unwrap();
        let c2 = c2.unwrap();

        if !ALL_CONSIDERED_CHARS.contains(&c1) || !ALL_CONSIDERED_CHARS.contains(&c2) {
            eprintln!("Invalid swap command.");
            eprintln!("Keys are not found in layout.");
            return;
        }

        self.append_to_history();

        if self.main_layout.swap_chars(c1, c2).is_none() {
            eprintln!("Failed to swap keys.");
        }
    }

    fn copy(&mut self, mut args: SplitWhitespace) {
        let from = args.next();
        let to = args.next();

        if from == Some("main") && to == Some("secondary") || from == Some("m") && to == Some("s") {
            self.secondary_layout = self.main_layout.clone();
        } else if from == Some("secondary") && to == Some("main")
            || from == Some("s") && to == Some("m")
        {
            self.append_to_history();
            self.main_layout = self.secondary_layout.clone();
        } else {
            eprintln!("Invalid copy command.");
            eprintln!("Use full forms: main/secondary or short forms: m/s but not mixed.");
        }
    }

    fn switch(&mut self) {
        let new_main = self.secondary_layout.clone();
        self.secondary_layout = self.main_layout.clone();
        self.main_layout = new_main;
    }

    fn print(&self, mut args: SplitWhitespace) {
        match args.next() {
            Some("main") | Some("m") | None => println!("{}", self.main_layout),
            Some("secondary") | Some("s") => println!("{}", self.secondary_layout),
            Some("qwerty") => println!("{}", Layout::qwerty()),
            Some("dvorak") => println!("{}", Layout::dvorak()),
            Some("last") | Some("l") => println!(
                "{}",
                self.layout_history.last().unwrap_or(&self.secondary_layout)
            ),
            Some(index_str) => {
                if let Ok(index) = index_str.parse::<usize>() {
                    if let Some(layout) = self.layout_history.get(index) {
                        println!("{}", layout);
                    } else {
                        eprintln!("No layout in history with the given index '{index_str}'. History length is {}.", self.layout_history.len())
                    }
                } else {
                    eprintln!("Unknown layout '{index_str}'.")
                }
            }
        }
    }
}

fn main() -> ExitCode {
    let state_result = AppState::restore_state();

    if let Err(err) = state_result {
        eprintln!("Restoring app state failed: {err}");
        eprintln!("Ensure that the 'ksed.json' file is valid json and has correct permissions.");
        return ExitCode::FAILURE;
    }

    let mut app_state = state_result.unwrap();

    if let Err(err) = app_state.run_app() {
        eprintln!("Running app CLI failed: {err}");
        return ExitCode::FAILURE;
    }

    if let Err(err) = app_state.save_state() {
        eprintln!("Saving app state failed: {err}");
        eprintln!("Ensure that the 'ksed.json' file can be written to.");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
