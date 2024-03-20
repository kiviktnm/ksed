use std::fmt::Display;

use crate::*;
use colored::{ColoredString, Colorize};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub(crate) struct Score {
    pub(crate) inner: u32,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub(crate) enum ScoreCategory {
    Horrible = 0,
    Bad = 2,
    Ok = 4,
    Good = 6,
    Excellent = 7,
    Awesome = 8,
}

impl Score {
    fn average(scores: &[Score]) -> Score {
        let mut sum = 0;
        let mut count = 0;
        for score in scores.iter() {
            sum += score.inner;
            count += 1;
        }
        let average = sum / count;

        Score { inner: average }
    }

    fn inc(&self) -> Score {
        Score {
            inner: (self.inner + 1).min(8),
        }
    }

    fn dec(&self) -> Score {
        Score {
            inner: if self.inner == 0 { 0 } else { self.inner - 1 },
        }
    }
}

impl From<ScoreCategory> for Score {
    fn from(value: ScoreCategory) -> Self {
        Score {
            inner: value as u32,
        }
    }
}

impl From<Score> for ScoreCategory {
    fn from(value: Score) -> Self {
        match value.inner {
            0 => ScoreCategory::Horrible,
            1 | 2 => ScoreCategory::Bad,
            3 | 4 => ScoreCategory::Ok,
            5 | 6 => ScoreCategory::Good,
            7 => ScoreCategory::Excellent,
            8 => ScoreCategory::Awesome,
            _ => ScoreCategory::Awesome,
        }
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ScoreCategory::from(*self))
    }
}

impl Display for ScoreCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ScoreCategory::Horrible => "Horrible ".on_red(),
            ScoreCategory::Bad => "Bad      ".red(),
            ScoreCategory::Ok => "Ok       ".yellow(),
            ScoreCategory::Good => "Good     ".green(),
            ScoreCategory::Excellent => "Excellent".cyan().bold(),
            ScoreCategory::Awesome => "Awesome  ".truecolor(0xae, 0x86, 0x25).bold(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
struct Layer {
    keys: [[char; 3]; 10],
}

impl Layer {
    fn new(layer_text_repr: &str) -> Result<Layer, Error> {
        let ltr_chars: Vec<_> = layer_text_repr.chars().collect();
        let mut keys = [['_'; 3]; 10];

        if ltr_chars.len() != 30 {
            return Err(Error::InvalidLayerTextReprLength);
        }

        let mut index = 0;
        for column in &mut keys {
            for key in column {
                let c = ltr_chars[index];
                if !ALL_CONSIDERED_CHARS.contains(&c) {
                    return Err(Error::InvalidLayerTextReprCharacter(c));
                }
                *key = c;
                index += 1;
            }
        }

        Ok(Layer { keys })
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Layout {
    primary_layer: Layer,
    symbols_layer: Layer,
}

impl Layout {
    pub(crate) fn new(
        primary_layer_text_repr: &str,
        symbols_layer_text_repr: &str,
    ) -> Result<Layout, Error> {
        let primary_layer = Layer::new(primary_layer_text_repr)?;
        let symbols_layer = Layer::new(symbols_layer_text_repr)?;

        Ok(Layout {
            primary_layer,
            symbols_layer,
        })
    }

    pub(crate) fn swap_chars(&mut self, c1: char, c2: char) -> Option<()> {
        let key1 = self.find_key(c1)?;
        let key2 = self.find_key(c2)?;

        if key1.0 == 0 {
            self.primary_layer.keys[key1.1][key1.2] = c2;
        } else if key1.0 == 1 {
            self.symbols_layer.keys[key1.1][key1.2] = c2;
        }

        if key2.0 == 0 {
            self.primary_layer.keys[key2.1][key2.2] = c1;
        } else if key2.0 == 1 {
            self.symbols_layer.keys[key2.1][key2.2] = c1;
        }

        Some(())
    }

    pub(crate) fn swap_keys(&mut self, key1: Key, key2: Key) {
        let c1 = if key1.0 == 0 {
            self.primary_layer.keys[key1.1][key1.2]
        } else {
            self.symbols_layer.keys[key1.1][key1.2]
        };

        let c2 = if key2.0 == 0 {
            self.primary_layer.keys[key2.1][key2.2]
        } else {
            self.symbols_layer.keys[key2.1][key2.2]
        };

        if key1.0 == 0 {
            self.primary_layer.keys[key1.1][key1.2] = c2;
        } else if key1.0 == 1 {
            self.symbols_layer.keys[key1.1][key1.2] = c2;
        }

        if key2.0 == 0 {
            self.primary_layer.keys[key2.1][key2.2] = c1;
        } else if key2.0 == 1 {
            self.symbols_layer.keys[key2.1][key2.2] = c1;
        }
    }

    fn find_key(&self, c: char) -> Option<Key> {
        for (x, column) in self.primary_layer.keys.iter().enumerate() {
            for (y, examined_key) in column.iter().enumerate() {
                if *examined_key == c {
                    return Some((0, x, y));
                }
            }
        }

        for (x, column) in self.symbols_layer.keys.iter().enumerate() {
            for (y, examined_key) in column.iter().enumerate() {
                if *examined_key == c {
                    return Some((1, x, y));
                }
            }
        }

        None
    }

    fn find_press(&self, c: char) -> Option<Press> {
        self.find_key(c).and_then(key_to_press)
    }

    pub(crate) fn evaluate_char(&self, c: &char) -> ScoreCategory {
        if let Some(key) = &self.find_key(*c) {
            if PRIMARY_KEYS.contains(key) {
                return ScoreCategory::Excellent;
            }
            if SECONDARY_KEYS.contains(key) {
                return ScoreCategory::Good;
            }
            if TERTIARY_KEYS.contains(key) {
                return ScoreCategory::Ok;
            }
            if QUATERNARY_KEYS.contains(key) {
                return ScoreCategory::Bad;
            }
        }
        ScoreCategory::Horrible
    }

    pub(crate) fn evaluate_bigram(&self, bigram: &Bigram) -> ScoreCategory {
        let mut score = Score::average(&[
            self.evaluate_char(&bigram.0).into(),
            self.evaluate_char(&bigram.1).into(),
        ]);

        if self.is_roll(&[bigram.0, bigram.1]) {
            score = score.inc();
        }

        let first_is_right_handed = self
            .find_key(bigram.0)
            .is_some_and(|key| self.is_right_handed(key));
        let second_is_right_handed = self
            .find_key(bigram.1)
            .is_some_and(|key| self.is_right_handed(key));

        if first_is_right_handed != second_is_right_handed {
            score = score.inc();
        } else if self
            .vertical_distance(bigram.0, bigram.1)
            .is_some_and(|distance| distance >= 2)
        {
            score = score.dec();
        }

        let first_press = self.find_press(bigram.0);
        let second_press = self.find_press(bigram.1);

        if first_press == second_press && bigram.0 != bigram.1 {
            score = score.dec();
        }

        score.into()
    }

    pub(crate) fn evaluate_trigram(&self, trigram: &Trigram) -> ScoreCategory {
        let mut score = Score::average(&[
            self.evaluate_char(&trigram.0).into(),
            self.evaluate_char(&trigram.1).into(),
            self.evaluate_char(&trigram.2).into(),
        ]);

        let first_is_right_handed = self
            .find_key(trigram.0)
            .is_some_and(|key| self.is_right_handed(key));
        let second_is_right_handed = self
            .find_key(trigram.1)
            .is_some_and(|key| self.is_right_handed(key));
        let third_is_right_handed = self
            .find_key(trigram.2)
            .is_some_and(|key| self.is_right_handed(key));

        if self.is_roll(&[trigram.0, trigram.1, trigram.2]) {
            score = score.inc();
        } else if first_is_right_handed == second_is_right_handed
            && second_is_right_handed == third_is_right_handed
        {
            score = score.dec();
        }

        if first_is_right_handed != second_is_right_handed
            && second_is_right_handed != third_is_right_handed
        {
            score = score.inc();
        }

        if first_is_right_handed == second_is_right_handed
            && self
                .vertical_distance(trigram.0, trigram.1)
                .is_some_and(|dist| dist >= 2)
        {
            score = score.dec();
        }
        if second_is_right_handed == third_is_right_handed
            && self
                .vertical_distance(trigram.1, trigram.2)
                .is_some_and(|dist| dist >= 2)
        {
            score = score.dec();
        }

        if (first_is_right_handed == second_is_right_handed)
            && (self.is_roll(&[trigram.0, trigram.1]))
            && (second_is_right_handed != third_is_right_handed)
        {
            score = score.inc();
        }

        if (second_is_right_handed == third_is_right_handed)
            && (self.is_roll(&[trigram.1, trigram.2]))
            && (first_is_right_handed != third_is_right_handed)
        {
            score = score.inc();
        }

        let first_press = self.find_press(trigram.0);
        let second_press = self.find_press(trigram.1);
        let third_press = self.find_press(trigram.2);

        if first_press == second_press && trigram.0 != trigram.1 {
            score = score.dec();
        }

        if second_press == third_press && trigram.1 != trigram.2 {
            score = score.dec();
        }

        if first_press == third_press && trigram.0 != trigram.2 {
            score = score.dec();
        }

        score.into()
    }

    pub(crate) fn calculate_word_stats(
        &self,
        words: &[WithOccurance<String>],
        total_word_count: f64,
        k: usize,
    ) -> WeightedStats {
        let mut total_chars = 0.0;

        let mut primary_keys_usage = 0.0;
        let mut secondary_keys_usage = 0.0;
        let mut tertiary_keys_usage = 0.0;
        let mut quaternary_keys_usage = 0.0;

        let mut right_hand_usage = 0.0;
        let mut hand_alteration_per_char = 0.0;

        let mut same_finger_repeats_per_char = 0.0;
        let mut same_finger_row_skips_per_char = 0.0;
        let mut same_hand_row_skips_per_char = 0.0;

        for word in &words[..k.min(words.len())] {
            let mut prev_key_opt = None;
            let mut prev_char = '_';

            for c in word.item.chars() {
                total_chars += 1.0 * word.occ() * total_word_count;

                let key_opt = self.find_key(c);

                if let Some(key) = key_opt {
                    if self.is_right_handed(key) {
                        right_hand_usage += 1.0 * word.occ() * total_word_count;
                    }

                    if PRIMARY_KEYS.contains(&key) {
                        primary_keys_usage += 1.0 * word.occ() * total_word_count;
                    }

                    if SECONDARY_KEYS.contains(&key) {
                        secondary_keys_usage += 1.0 * word.occ() * total_word_count;
                    }

                    if TERTIARY_KEYS.contains(&key) {
                        tertiary_keys_usage += 1.0 * word.occ() * total_word_count;
                    }

                    if QUATERNARY_KEYS.contains(&key) {
                        quaternary_keys_usage += 1.0 * word.occ() * total_word_count;
                    }

                    if let Some(prev_key) = prev_key_opt {
                        if self.is_right_handed(key) != self.is_right_handed(prev_key) {
                            hand_alteration_per_char += word.occ() * total_word_count;
                        }

                        if self.find_press(c) == self.find_press(prev_char) && c != prev_char {
                            same_finger_repeats_per_char += 1.0 * word.occ() * total_word_count;
                        }

                        if self.vertical_distance(c, prev_char) >= Some(2)
                            && self.is_right_handed(key) == self.is_right_handed(prev_key)
                        {
                            if self.find_press(c) == self.find_press(prev_char) {
                                same_finger_row_skips_per_char +=
                                    1.0 * word.occ() * total_word_count;
                            }

                            same_hand_row_skips_per_char += 1.0 * word.occ() * total_word_count;
                        }
                    }
                }

                prev_key_opt = key_opt;
                prev_char = c;
            }
        }

        same_hand_row_skips_per_char /= total_chars / 100.0;
        same_finger_row_skips_per_char /= total_chars / 100.0;
        same_finger_repeats_per_char /= total_chars / 100.0;

        right_hand_usage /= total_chars / 100.0;
        hand_alteration_per_char /= total_chars / 100.0;

        primary_keys_usage /= total_chars / 100.0;
        secondary_keys_usage /= total_chars / 100.0;
        tertiary_keys_usage /= total_chars / 100.0;
        quaternary_keys_usage /= total_chars / 100.0;

        WeightedStats {
            primary_keys_usage,
            secondary_keys_usage,
            tertiary_keys_usage,
            quaternary_keys_usage,
            right_hand_usage,
            hand_alteration_per_char,
            same_finger_repeats_per_char,
            same_finger_row_skips_per_char,
            same_hand_row_skips_per_char,
        }
    }

    fn vertical_distance(&self, c1: char, c2: char) -> Option<usize> {
        self.find_key(c1)
            .and_then(|key1| self.find_key(c2).map(|key2| key1.2.abs_diff(key2.2)))
    }

    fn is_roll(&self, text: &[char]) -> bool {
        let mut smallest_y = 10;
        let mut largest_y = 0;

        let mut prev_key_opt: Option<Key> = None;
        for c in text {
            let key_opt = self.find_key(*c);

            if let Some(key) = key_opt {
                smallest_y = key.2.min(smallest_y);
                largest_y = key.2.max(largest_y);

                if let Some(prev_key) = prev_key_opt {
                    if self.is_right_handed(key) != self.is_right_handed(prev_key) {
                        return false;
                    }
                    if prev_key.0 != key.0 {
                        return false;
                    }
                    if key_to_press(key) == key_to_press(prev_key) {
                        return false;
                    }
                    if self.is_right_handed(key) {
                        if prev_key.1 <= key.1 {
                            return false;
                        }
                    } else if prev_key.1 >= key.1 {
                        return false;
                    }
                }
            }

            prev_key_opt = key_opt;
        }

        largest_y - smallest_y <= 1
    }

    fn is_right_handed(&self, key: Key) -> bool {
        matches!(key_to_press(key), Some(Press::Right(_)))
    }

    pub(crate) const fn qwerty() -> Layout {
        Layout {
            primary_layer: Layer {
                keys: [
                    ['q', 'a', 'z'],
                    ['w', 's', 'x'],
                    ['e', 'd', 'c'],
                    ['r', 'f', 'v'],
                    ['t', 'g', 'b'],
                    ['y', 'h', 'n'],
                    ['u', 'j', 'm'],
                    ['i', 'k', ','],
                    ['o', 'l', '.'],
                    ['p', ';', '/'],
                ],
            },
            symbols_layer: Layer {
                keys: [
                    ['%', '!', '$'],
                    ['@', '#', '€'],
                    ['{', '(', '['],
                    ['}', ')', ']'],
                    ['|', '\'', '&'],
                    ['´', '*', '\\'],
                    ['`', '/', '-'],
                    ['¨', '=', '^'],
                    ['~', 'ä', '.'],
                    ['å', 'ö', '/'],
                ],
            },
        }
    }

    pub(crate) const fn dvorak() -> Layout {
        Layout {
            primary_layer: Layer {
                keys: [
                    ['ä', 'a', ';'],
                    [',', 'o', 'q'],
                    ['.', 'e', 'j'],
                    ['p', 'u', 'k'],
                    ['y', 'i', 'x'],
                    ['f', 'd', 'b'],
                    ['g', 'h', 'm'],
                    ['c', 't', 'w'],
                    ['r', 'n', 'v'],
                    ['l', 's', 'z'],
                ],
            },
            symbols_layer: Layer {
                keys: [
                    ['%', '!', '$'],
                    ['@', '#', '€'],
                    ['{', '(', '['],
                    ['}', ')', ']'],
                    ['|', '\'', '&'],
                    ['´', '*', '\\'],
                    ['`', '/', '-'],
                    ['¨', '=', '^'],
                    ['~', 'ä', '.'],
                    ['å', 'ö', '/'],
                ],
            },
        }
    }

    fn color_c(&self, c: char) -> ColoredString {
        let text = c.to_string();
        if let Some(key) = self.find_key(c) {
            if PRIMARY_KEYS.contains(&key) {
                text.cyan().bold()
            } else if SECONDARY_KEYS.contains(&key) {
                text.green().bold()
            } else if TERTIARY_KEYS.contains(&key) {
                text.yellow()
            } else if QUATERNARY_KEYS.contains(&key) {
                text.red()
            } else {
                text.on_red()
            }
        } else {
            text.normal()
        }
    }
}

impl Display for Layout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", "Layer 1:".bold())?;

        for (index, column) in self.primary_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[0];

            write!(f, "{} ", self.color_c(c))?;
        }

        writeln!(f)?;

        for (index, column) in self.primary_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[1];

            write!(f, "{} ", self.color_c(c))?;
        }

        writeln!(f)?;

        for (index, column) in self.primary_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[2];

            write!(f, "{} ", self.color_c(c))?;
        }

        writeln!(f)?;

        writeln!(f, "{}", "Layer 2:".bold())?;

        for (index, column) in self.symbols_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[0];

            write!(f, "{} ", c)?;
        }

        writeln!(f)?;

        for (index, column) in self.symbols_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[1];

            write!(f, "{} ", c)?;
        }

        writeln!(f)?;

        for (index, column) in self.symbols_layer.keys.iter().enumerate() {
            if index == 5 {
                write!(f, "    ")?;
            }
            let c = column[2];

            write!(f, "{} ", c)?;
        }

        Ok(())
    }
}
