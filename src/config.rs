use crate::Finger::*;
use crate::Key;
use crate::Press::*;

pub(crate) const ALL_CONSIDERED_CHARS: [char; 58] = [
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
    ';', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', '%', '@', '{', '}', '|', '´', '`', '¨',
    '~', 'å', '!', '#', '(', ')', '\'', '*', '/', '=', 'ä', 'ö', '$', '€', '[', ']', '&', '\\',
    '-', '^',
];

pub(crate) const SPECIAL_CHARS: [char; 27] = [
    ';', '/', '%', '@', '{', '}', '|', '´', '`', '¨', '~', '!', '#', '(', ')', '\'', '*', '/', '=',
    '$', '€', '[', ']', '&', '\\', '-', '^',
];

pub(crate) const NORMAL_CHARS: [char; 31] = [
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
    'z', 'x', 'c', 'v', 'b', 'n', 'm', 'ä', 'ö', '.', ',', ';',
];

pub(crate) const LETTERS: [char; 28] = [
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l',
    'z', 'x', 'c', 'v', 'b', 'n', 'm', 'ä', 'ö',
];

pub(crate) const VOWELS: [char; 8] = ['e', 'y', 'u', 'i', 'o', 'a', 'ä', 'ö'];

pub(crate) const HAND_ASSIGNMENTS: [[[crate::Press; 3]; 10]; 2] = [
    [
        [Left(Ring), Left(Pinky), Left(Pinky)],
        [Left(Ring), Left(Ring), Left(Ring)],
        [Left(Middle), Left(Middle), Left(Middle)],
        [Left(Index), Left(Index), Left(Index)],
        [Left(Index), Left(Index), Left(Index)],
        [Right(Index), Right(Index), Right(Index)],
        [Right(Index), Right(Index), Right(Index)],
        [Right(Middle), Right(Middle), Right(Middle)],
        [Right(Ring), Right(Ring), Right(Ring)],
        [Right(Ring), Right(Pinky), Right(Pinky)],
    ],
    [
        [Left(Ring), Left(Pinky), Left(Pinky)],
        [Left(Ring), Left(Ring), Left(Ring)],
        [Left(Middle), Left(Middle), Left(Middle)],
        [Left(Index), Left(Index), Left(Index)],
        [Left(Index), Left(Index), Left(Index)],
        [Right(Index), Right(Index), Right(Index)],
        [Right(Index), Right(Index), Right(Index)],
        [Right(Middle), Right(Middle), Right(Middle)],
        [Right(Ring), Right(Ring), Right(Ring)],
        [Right(Ring), Right(Pinky), Right(Pinky)],
    ],
];

pub(crate) const PRIMARY_KEYS: [Key; 8] = [
    (0, 0, 1),
    (0, 1, 1),
    (0, 2, 1),
    (0, 3, 1),
    (0, 6, 1),
    (0, 7, 1),
    (0, 8, 1),
    (0, 9, 1),
];

pub(crate) const SECONDARY_KEYS: [Key; 6] = [
    (0, 2, 0),
    (0, 3, 0),
    (0, 6, 0),
    (0, 7, 0),
    (0, 3, 2),
    (0, 6, 2),
];

pub(crate) const TERTIARY_KEYS: [Key; 6] = [
    (0, 1, 0),
    (0, 8, 0),
    (0, 2, 2),
    (0, 4, 1),
    (0, 7, 2),
    (0, 5, 1),
];

pub(crate) const QUATERNARY_KEYS: [Key; 6] = [
    (0, 1, 2),
    (0, 8, 2),
    (0, 4, 0),
    (0, 5, 0),
    (0, 4, 2),
    (0, 5, 2),
];

pub(crate) const REST_OF_THE_KEYS: [Key; 4] = [(0, 0, 0), (0, 0, 2), (0, 9, 0), (0, 9, 2)];

pub(crate) const PINNED: [Key; 3] = [(0, 4, 0), (0, 5, 0), (0, 9, 2)];
