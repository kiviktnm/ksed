use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    hash::Hash,
    io::{self, BufRead, BufReader},
};

use colored::Colorize;
use serde::{Deserialize, Serialize};
pub(crate) type Bigram = (char, char);
pub(crate) type Trigram = (char, char, char);

#[derive(Clone, Debug)]
pub(crate) struct WithOccurance<T> {
    pub(crate) item: T,
    occurance: u64,
}

use crate::config::*;

#[derive(Debug, Default)]
pub(crate) struct WeightedStats {
    pub(crate) primary_keys_usage: f64,
    pub(crate) secondary_keys_usage: f64,
    pub(crate) tertiary_keys_usage: f64,
    pub(crate) quaternary_keys_usage: f64,
    pub(crate) right_hand_usage: f64,
    pub(crate) hand_alteration_per_char: f64,
    pub(crate) same_finger_repeats_per_char: f64,
    pub(crate) same_finger_row_skips_per_char: f64,
    pub(crate) same_hand_row_skips_per_char: f64,
}

impl Display for WeightedStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}{:.4}",
            "Primary Keys (%):                       ".bold().cyan(),
            self.primary_keys_usage
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Secondary Keys (%):                     ".italic().green(),
            self.secondary_keys_usage
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Tertiary Keys (%):                      ".bold().yellow(),
            self.tertiary_keys_usage
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Quaternary Keys (%):                    ".italic(),
            self.quaternary_keys_usage
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Right Hand Share (%):                   ".bold().blue(),
            self.right_hand_usage
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Hand Alteration / Char (%):             ".italic().green(),
            self.hand_alteration_per_char
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Finger Repeats / Char (%):              ".bold().red(),
            self.same_finger_repeats_per_char
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Finger Row Skips / Char (%):            ".italic().red(),
            self.same_finger_row_skips_per_char
        )?;
        writeln!(
            f,
            "{}{:.4}",
            "Hand Row Skips / Char (%):              ".bold().red(),
            self.same_hand_row_skips_per_char
        )
    }
}

const OCCURANCE_STORING_COEF: u64 = 1_000_000_000_000;

impl<T> WithOccurance<T> {
    pub(crate) fn new(item: T, count: u64, total: u64) -> WithOccurance<T> {
        WithOccurance {
            item,
            occurance: count * (OCCURANCE_STORING_COEF / total),
        }
    }

    pub(crate) fn occ(&self) -> f64 {
        self.occurance as f64 / (OCCURANCE_STORING_COEF as f64 / 100.0)
    }
}

impl<T> WithOccurance<T>
where
    T: Clone + Hash + Eq,
{
    fn sorted_items<'a, I>(iterator: I, total: u64) -> Vec<WithOccurance<T>>
    where
        I: Iterator<Item = (&'a T, &'a u64)>,
        T: 'static,
    {
        let mut items_with_occurance: Vec<_> = iterator
            .map(|(item, count)| WithOccurance::new(item.clone(), *count, total))
            .collect();

        items_with_occurance
            .sort_unstable_by_key(|WithOccurance { item: _, occurance }| *occurance);

        items_with_occurance
    }

    fn combined_sorted_items<'a, I>(iters: Vec<(I, u64)>) -> Vec<WithOccurance<T>>
    where
        I: Iterator<Item = (&'a T, &'a u64)>,
        T: 'static,
    {
        let mut item_occurances = HashMap::new();
        let mut total_items_combined = 0;

        for (_, total_items_in_iter) in &iters {
            total_items_combined += total_items_in_iter;
        }

        for (i, _) in iters {
            for (item, count) in i {
                let occ = WithOccurance::new(item.clone(), *count, total_items_combined);

                *item_occurances.entry(item.clone()).or_insert(0) += occ.occurance;
            }
        }

        let mut sorted: Vec<_> = item_occurances
            .into_iter()
            .map(|(item, occurance)| WithOccurance { item, occurance })
            .collect();

        sorted.sort_unstable_by_key(|WithOccurance { item: _, occurance }| *occurance);

        sorted
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct CharGroup {
    c1: char,
    c2: char,
    c3: char,
}

impl CharGroup {
    pub(crate) fn new(a: char, b: char, c: char) -> CharGroup {
        let mut sorted = [a, b, c];
        sorted.sort_unstable();

        CharGroup {
            c1: sorted[0],
            c2: sorted[1],
            c3: sorted[2],
        }
    }

    pub(crate) fn contains(&self, c: char) -> bool {
        self.c1 == c || self.c2 == c || self.c3 == c
    }

    fn get_all_letter_chargroups() -> Vec<CharGroup> {
        let mut chargroups = Vec::new();

        for a in NORMAL_CHARS {
            for b in NORMAL_CHARS {
                for c in NORMAL_CHARS {
                    if a < b && b < c {
                        chargroups.push(CharGroup::new(a, b, c));
                    }
                }
            }
        }

        chargroups
    }
}

impl Display for CharGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{}{})", self.c1, self.c2, self.c3)
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub(crate) struct CharDataset {
    char_counts: HashMap<char, u64>,
    total_chars: u64,

    #[serde(skip)]
    sorted_chars: Option<Vec<WithOccurance<char>>>,

    bigram_counts: HashMap<Bigram, u64>,
    total_bigrams: u64,

    #[serde(skip)]
    sorted_bigrams: Option<Vec<WithOccurance<Bigram>>>,

    trigram_counts: HashMap<Trigram, u64>,
    total_trigrams: u64,

    #[serde(skip)]
    sorted_trigrams: Option<Vec<WithOccurance<Trigram>>>,

    letter_chargroup_counts: HashMap<CharGroup, u64>,
    total_chargroups: u64,

    #[serde(skip)]
    sorted_chargroups: Option<Vec<WithOccurance<CharGroup>>>,

    word_counts: HashMap<String, u64>,
    total_words: u64,

    #[serde(skip)]
    sorted_words: Option<Vec<WithOccurance<String>>>,
}

impl CharDataset {
    pub(crate) fn create() -> CharDataset {
        let mut chargroup_counts = HashMap::new();
        let mut total_chargroups = 0;

        for chargroup in CharGroup::get_all_letter_chargroups() {
            chargroup_counts.insert(chargroup, 0);
            total_chargroups += 1;
        }

        CharDataset {
            letter_chargroup_counts: chargroup_counts,
            total_chargroups,
            ..Default::default()
        }
    }

    pub(crate) fn parse_file(&mut self, filename: &str) -> Result<(), io::Error> {
        let file = File::open(filename)?;
        let mut reader = BufReader::new(file);

        let mut line = String::new();
        while reader.read_line(&mut line)? != 0 {
            self.parse_line(&line);
            line.clear();
        }

        Ok(())
    }

    pub(crate) fn parse_line(&mut self, line: &str) {
        self.sorted_words = None;
        self.sorted_chargroups = None;
        self.sorted_bigrams = None;
        self.sorted_trigrams = None;
        self.sorted_chars = None;

        let mut prev_prev_char = None;
        let mut prev_char = None;

        let mut word = String::new();

        for current_char in line.chars() {
            let current_char = current_char.to_lowercase().next().unwrap_or('0');

            if current_char == ' ' && !word.trim().is_empty() {
                *self.word_counts.entry(word).or_insert(0) += 1;
                self.total_words += 1;
                word = String::new();
            }

            if !ALL_CONSIDERED_CHARS.contains(&current_char) {
                continue;
            }

            if let Some(prev_char) = prev_char {
                let bigram = (prev_char, current_char);

                *self.bigram_counts.entry(bigram).or_insert(0) += 1;
                self.total_bigrams += 1;

                if let Some(prev_prev_char) = prev_prev_char {
                    let trigram = (prev_prev_char, prev_char, current_char);

                    *self.trigram_counts.entry(trigram).or_insert(0) += 1;
                    self.total_trigrams += 1;

                    if NORMAL_CHARS.contains(&prev_prev_char)
                        && NORMAL_CHARS.contains(&prev_char)
                        && NORMAL_CHARS.contains(&current_char)
                    {
                        let chargroup = CharGroup::new(prev_prev_char, prev_char, current_char);
                        *self.letter_chargroup_counts.entry(chargroup).or_insert(0) += 1;
                        self.total_chargroups += 1;
                    }
                }
            }

            *self.char_counts.entry(current_char).or_insert(0) += 1;
            self.total_chars += 1;

            word.push(current_char);

            prev_prev_char = prev_char;
            prev_char = Some(current_char);
        }

        if !word.trim().is_empty() {
            *self.word_counts.entry(word).or_insert(0) += 1;
            self.total_words += 1;
        }
    }

    pub(crate) fn sorted_chars(&mut self) -> Vec<WithOccurance<char>> {
        if let Some(presorted) = &self.sorted_chars {
            return presorted.clone();
        }

        let mut sorted = WithOccurance::sorted_items(self.char_counts.iter(), self.total_chars);
        sorted.reverse();

        self.sorted_chars = Some(sorted.clone());

        sorted
    }

    pub(crate) fn sorted_bigrams(&mut self) -> Vec<WithOccurance<Bigram>> {
        if let Some(presorted) = &self.sorted_bigrams {
            return presorted.clone();
        }

        let mut sorted = WithOccurance::sorted_items(self.bigram_counts.iter(), self.total_bigrams);
        sorted.reverse();

        self.sorted_bigrams = Some(sorted.clone());

        sorted
    }

    pub(crate) fn sorted_trigrams(&mut self) -> Vec<WithOccurance<Trigram>> {
        if let Some(presorted) = &self.sorted_trigrams {
            return presorted.clone();
        }

        let mut sorted =
            WithOccurance::sorted_items(self.trigram_counts.iter(), self.total_trigrams);

        sorted.reverse();

        self.sorted_trigrams = Some(sorted.clone());

        sorted
    }

    pub(crate) fn sorted_chargroups(&mut self) -> Vec<WithOccurance<CharGroup>> {
        if let Some(presorted) = &self.sorted_chargroups {
            return presorted.clone();
        }

        let mut sorted =
            WithOccurance::sorted_items(self.letter_chargroup_counts.iter(), self.total_chargroups);

        sorted.reverse();

        self.sorted_chargroups = Some(sorted.clone());

        sorted
    }

    pub(crate) fn sorted_special_chars(&mut self) -> Vec<WithOccurance<char>> {
        let mut sorted = WithOccurance::sorted_items(
            self.char_counts
                .iter()
                .filter(|(char, _)| SPECIAL_CHARS.contains(char)),
            self.char_counts
                .iter()
                .filter(|(char, _)| SPECIAL_CHARS.contains(char))
                .fold(0, |acc, (_, count)| acc + count),
        );
        sorted.reverse();

        sorted
    }

    pub(crate) fn sorted_vowel_bigrams(&mut self) -> Vec<WithOccurance<Bigram>> {
        let mut sorted = WithOccurance::sorted_items(
            self.bigram_counts
                .iter()
                .filter(|(bigram, _)| VOWELS.contains(&bigram.0) && VOWELS.contains(&bigram.1)),
            self.total_bigrams,
        );
        sorted.reverse();

        sorted
    }

    pub(crate) fn sorted_consonant_bigrams(&mut self) -> Vec<WithOccurance<Bigram>> {
        let mut sorted = WithOccurance::sorted_items(
            self.bigram_counts.iter().filter(|(bigram, _)| {
                !VOWELS.contains(&bigram.0)
                    && !VOWELS.contains(&bigram.1)
                    && LETTERS.contains(&bigram.0)
                    && LETTERS.contains(&bigram.1)
            }),
            self.total_bigrams,
        );
        sorted.reverse();

        sorted
    }

    pub(crate) fn sorted_words(&mut self) -> Vec<WithOccurance<String>> {
        if let Some(presorted) = &self.sorted_words {
            return presorted.clone();
        }

        let mut sorted = WithOccurance::sorted_items(self.word_counts.iter(), self.total_words);
        sorted.reverse();

        self.sorted_words = Some(sorted.clone());

        sorted
    }

    pub(crate) fn word_count(&self) -> u64 {
        self.total_words
    }

    pub(crate) fn unique_word_count(&self) -> usize {
        self.word_counts.len()
    }

    pub(crate) fn combined_sorted_chars(datasets: Vec<&CharDataset>) -> Vec<WithOccurance<char>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((dataset.char_counts.iter(), dataset.total_chars));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_bigrams(
        datasets: Vec<&CharDataset>,
    ) -> Vec<WithOccurance<Bigram>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((dataset.bigram_counts.iter(), dataset.total_bigrams));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_vowel_bigrams(
        datasets: Vec<&CharDataset>,
    ) -> Vec<WithOccurance<Bigram>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((
                dataset
                    .bigram_counts
                    .iter()
                    .filter(|(bigram, _)| VOWELS.contains(&bigram.0) && VOWELS.contains(&bigram.1)),
                dataset.total_bigrams,
            ));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_consonant_bigrams(
        datasets: Vec<&CharDataset>,
    ) -> Vec<WithOccurance<Bigram>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((
                dataset.bigram_counts.iter().filter(|(bigram, _)| {
                    !VOWELS.contains(&bigram.0)
                        && !VOWELS.contains(&bigram.1)
                        && LETTERS.contains(&bigram.0)
                        && LETTERS.contains(&bigram.1)
                }),
                dataset.total_bigrams,
            ));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_trigrams(
        datasets: Vec<&CharDataset>,
    ) -> Vec<WithOccurance<Trigram>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((dataset.trigram_counts.iter(), dataset.total_trigrams));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_chargroups(
        datasets: Vec<&CharDataset>,
    ) -> Vec<WithOccurance<CharGroup>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((
                dataset.letter_chargroup_counts.iter(),
                dataset.total_chargroups,
            ));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);

        sorted.reverse();

        sorted
    }

    pub(crate) fn combined_sorted_words(datasets: Vec<&CharDataset>) -> Vec<WithOccurance<String>> {
        let mut iters = Vec::new();

        for dataset in datasets {
            iters.push((dataset.word_counts.iter(), dataset.total_words));
        }

        let mut sorted = WithOccurance::combined_sorted_items(iters);
        sorted.reverse();

        sorted
    }
}
