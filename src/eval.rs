use std::sync::{Arc, Mutex};

use crate::*;
use rayon::prelude::*;

pub(crate) struct Evaluator<'a> {
    verified_bad_layouts: Arc<Mutex<&'a mut HashSet<Layout>>>,
    words: Vec<WithOccurance<String>>,
    bigrams: Vec<WithOccurance<Bigram>>,
    trigrams: Vec<WithOccurance<Trigram>>,
    total_words: f64,
    bigram_n: usize,
    trigram_n: usize,
    word_k: usize,
}

impl<'a> Evaluator<'a> {
    pub(crate) fn new(
        datasets: &Vec<&CharDataset>,
        n: usize,
        word_k: usize,
        verified_bad_layouts: &'a mut HashSet<Layout>,
    ) -> Evaluator<'a> {
        let mut total_words: f64 = 0.0;

        for dataset in datasets {
            total_words += dataset.word_count() as f64;
        }

        let words = CharDataset::combined_sorted_words(datasets.clone());
        let bigrams = CharDataset::combined_sorted_bigrams(datasets.clone());
        let trigrams = CharDataset::combined_sorted_trigrams(datasets.clone());

        let bigram_n = n.min(bigrams.len());
        let trigram_n = n.min(trigrams.len());

        Evaluator {
            words,
            bigrams,
            trigrams,
            total_words,
            bigram_n,
            trigram_n,
            word_k,
            verified_bad_layouts: Arc::new(Mutex::new(verified_bad_layouts)),
        }
    }

    pub(crate) fn improve_layout(&self, layout: Layout, branches: u8) -> Layout {
        let root_branches = 16;
        let mut best_options = Vec::new();

        (0..root_branches)
            .into_par_iter()
            .map(|thread_num| {
                println!("Thread {thread_num} started.");

                let result = self.improve_layout_recursively(
                    layout.clone(),
                    branches as i32,
                    true,
                    0,
                    thread_num,
                );

                println!("Thread {thread_num} completed.");

                result
            })
            .collect_into_vec(&mut best_options);

        let mut best_layout = layout;
        println!("All threads completed. Evaluating options.");

        for new_layout in best_options {
            if self.is_first_layout_better_uncompromising(&new_layout, &best_layout) {
                {
                    self.verified_bad_layouts
                        .lock()
                        .unwrap()
                        .insert(best_layout);
                }
                best_layout = new_layout;
            } else {
                self.verified_bad_layouts.lock().unwrap().insert(new_layout);
            }
        }

        best_layout
    }

    pub(crate) fn single_thread_improve_layout(&self, layout: Layout, branches: u8) -> Layout {
        self.improve_layout_recursively(layout, branches.into(), false, 0, 0)
    }

    fn improve_layout_recursively(
        &self,
        mut layout: Layout,
        branches: i32,
        should_print_updates: bool,
        depth: usize,
        thread_num: usize,
    ) -> Layout {
        if branches <= 0 || depth > 100 {
            let result;

            {
                result =
                    swap_keys_randomly(layout, &self.verified_bad_layouts.lock().unwrap(), false);
            }

            return result;
        }

        for n in 0..branches {
            let mut option;

            {
                option = swap_keys_randomly(
                    layout.clone(),
                    &self.verified_bad_layouts.lock().unwrap(),
                    true,
                );
            }

            let in_bad_layouts;

            {
                in_bad_layouts = self.verified_bad_layouts.lock().unwrap().contains(&option);
            }

            if !self.is_first_layout_better(&option, &layout) || in_bad_layouts {
                option = self.improve_layout_recursively(
                    option,
                    branches - 1,
                    false,
                    depth + 1,
                    thread_num,
                );
            } else {
                option =
                    self.improve_layout_recursively(option, branches, false, depth + 1, thread_num);
            }

            if self.is_first_layout_better_uncompromising(&option, &layout) {
                {
                    self.verified_bad_layouts
                        .lock()
                        .unwrap()
                        .insert(layout.clone());
                }
                layout = option;

                if should_print_updates {
                    println!(
                        "{:02} -> {}/{}. Found improvements!",
                        thread_num,
                        n + 1,
                        branches
                    );
                }
            } else {
                self.verified_bad_layouts
                    .lock()
                    .unwrap()
                    .insert(option.clone());

                if should_print_updates {
                    println!(
                        "{:02} -> {}/{}. No improvements.",
                        thread_num,
                        n + 1,
                        branches
                    );
                }
            }
        }

        layout
    }

    pub(crate) fn alt_improve_layout(
        &self,
        initial_layout: Layout,
        rounds: u16,
        depth: u16,
        initial_depth: u16,
    ) -> Layout {
        let root_branches = 16;

        let mut best_so_far = initial_layout.clone();
        let mut best_options = Vec::new();

        println!("Improving for {rounds} loop(s).");

        for r in 1..=rounds {
            (0..root_branches)
                .into_par_iter()
                .map(|_| {
                    let mut inner_best_so_far = best_so_far.clone();
                    let mut mix = inner_best_so_far.clone();

                    {
                        let verified_bad_layouts = &self.verified_bad_layouts.lock().unwrap();
                        for _ in 0..initial_depth {
                            mix = swap_keys_randomly(mix, verified_bad_layouts, true);
                        }
                    }

                    let mut mixes = depth;

                    while mixes > 0 {
                        {
                            mix = swap_keys_randomly(
                                mix,
                                &self.verified_bad_layouts.lock().unwrap(),
                                false,
                            );
                        }
                        if self.is_first_layout_better_uncompromising(&mix, &inner_best_so_far) {
                            {
                                self.verified_bad_layouts
                                    .lock()
                                    .unwrap()
                                    .insert(inner_best_so_far);
                            }
                            inner_best_so_far = mix.clone();
                            mixes += 1;
                        } else {
                            mixes -= 1;
                            {
                                self.verified_bad_layouts
                                    .lock()
                                    .unwrap()
                                    .insert(mix.clone());
                            }
                        }
                    }
                    inner_best_so_far
                })
                .collect_into_vec(&mut best_options);

            for layout in best_options.drain(..) {
                if self.is_first_layout_better_uncompromising(&layout, &best_so_far) {
                    best_so_far = layout;
                    {
                        self.verified_bad_layouts
                            .lock()
                            .unwrap()
                            .insert(best_so_far.clone());
                    }
                } else {
                    self.verified_bad_layouts.lock().unwrap().insert(layout);
                }
            }

            if best_so_far != initial_layout {
                println!("-> {r}/{rounds}: Improvements found.");
                println!("{best_so_far}");
            } else {
                println!("-> {r}/{rounds}.");
            }
        }

        best_so_far
    }

    pub(crate) fn is_first_layout_better_uncompromising(
        &self,
        first: &Layout,
        second: &Layout,
    ) -> bool {
        let (
            layout1_positive_score,
            layout1_negative_score,
            layout2_positive_score,
            layout2_negative_score,
        ) = self.calculate_scores(first, second);

        if layout1_negative_score <= layout2_negative_score
            && layout1_positive_score >= layout2_positive_score
        {
            return true;
        }

        false
    }

    pub(crate) fn is_first_layout_better(&self, first: &Layout, second: &Layout) -> bool {
        let (
            layout1_positive_score,
            layout1_negative_score,
            layout2_positive_score,
            layout2_negative_score,
        ) = self.calculate_scores(first, second);

        if layout1_negative_score <= layout2_negative_score
            && layout1_positive_score >= layout2_positive_score
        {
            return true;
        }

        if layout1_negative_score >= layout2_negative_score
            && layout1_positive_score <= layout2_positive_score
        {
            return false;
        }

        let negative_score_diff_perc =
            (layout1_negative_score - layout2_negative_score) / layout2_negative_score;
        let positive_score_diff_perc =
            (layout1_positive_score - layout2_positive_score) / layout2_positive_score;

        if positive_score_diff_perc - negative_score_diff_perc > 0.0 {
            return true;
        }

        false
    }

    fn calculate_scores(&self, first: &Layout, second: &Layout) -> (f64, f64, f64, f64) {
        let mut layout1_scores = HashMap::new();
        let mut layout2_scores = HashMap::new();

        for bigram in &self.bigrams[..self.bigram_n] {
            let layout1_score = first.evaluate_bigram(&bigram.item);
            *layout1_scores.entry(layout1_score).or_insert(0.0) += bigram.occ();

            let layout2_score = second.evaluate_bigram(&bigram.item);
            *layout2_scores.entry(layout2_score).or_insert(0.0) += bigram.occ();
        }

        for trigram in &self.trigrams[..self.trigram_n] {
            let layout1_score = first.evaluate_trigram(&trigram.item);
            *layout1_scores.entry(layout1_score).or_insert(0.0) += trigram.occ();

            let layout2_score = second.evaluate_trigram(&trigram.item);
            *layout2_scores.entry(layout2_score).or_insert(0.0) += trigram.occ();
        }

        let layout1_word_stats =
            first.calculate_word_stats(&self.words, self.total_words, self.word_k);
        let layout2_word_stats =
            second.calculate_word_stats(&self.words, self.total_words, self.word_k);

        let layout1_negative_score = word_stats_negative_score(&layout1_word_stats);
        let layout2_negative_score = word_stats_negative_score(&layout2_word_stats);

        let mut layout1_positive_score = 0.0;

        for (score, count) in layout1_scores {
            layout1_positive_score += f64::from(Score::from(score).inner) * count;
        }

        let mut layout2_positive_score = 0.0;

        for (score, count) in layout2_scores {
            layout2_positive_score += f64::from(Score::from(score).inner) * count;
        }

        (
            layout1_positive_score,
            layout1_negative_score,
            layout2_positive_score,
            layout2_negative_score,
        )
    }
}

fn swap_keys_randomly(
    mut layout: Layout,
    bad_layouts: &HashSet<Layout>,
    allowed_in_bad_layouts: bool,
) -> Layout {
    let mut loop_count: u64 = 0;
    let mut swap_count = 1;
    let original_layout = layout.clone();
    loop {
        layout = original_layout.clone();

        if loop_count >= 10_000 {
            swap_count += 1;
            loop_count = 0;
        }

        for _ in 0..swap_count {
            let selection = fastrand::u32(..5);
            match selection {
                0 => swap_keys_randomly_from(&mut layout, &REST_OF_THE_KEYS),
                1 => swap_keys_randomly_from(&mut layout, &QUATERNARY_KEYS),
                2 => swap_keys_randomly_from(&mut layout, &TERTIARY_KEYS),
                3 => swap_keys_randomly_from(&mut layout, &SECONDARY_KEYS),
                _ => swap_keys_randomly_from(&mut layout, &PRIMARY_KEYS),
            };
        }

        if !bad_layouts.contains(&layout) || allowed_in_bad_layouts {
            return layout;
        }
        loop_count += 1;
    }
}

fn swap_keys_randomly_from(layout: &mut Layout, from: &[Key]) {
    let mut index1 = fastrand::usize(..from.len());
    let mut index2 = fastrand::usize(..from.len());

    while PINNED.contains(&from[index1]) {
        index1 = fastrand::usize(..from.len());
    }

    while index1 == index2 || PINNED.contains(&from[index2]) {
        index2 = fastrand::usize(..from.len());
    }

    let key1 = from[index1];
    let key2 = from[index2];

    layout.swap_keys(key1, key2);
}

fn word_stats_negative_score(stats: &WeightedStats) -> f64 {
    stats.same_hand_row_skips_per_char + (50.0 - stats.right_hand_usage).abs()
        - stats.hand_alteration_per_char
        + stats
            .same_finger_repeats_per_char
            .powi(3)
            .max(stats.same_finger_repeats_per_char)
            * 5.0
        + stats
            .same_finger_row_skips_per_char
            .powi(3)
            .max(stats.same_finger_repeats_per_char)
            * 7.0
}
