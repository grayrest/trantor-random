//! xoshiro256++ and its SplitMix64 seeding, from `rand_xoshiro` 0.8.1.

use rand_xoshiro::Xoshiro256PlusPlus;
use rand_xoshiro::rand_core::{Rng, SeedableRng};

use crate::{def, roc};

const IN: &str = "\t\t\t";
const WORDS: usize = 40;

fn words_of(state: [u8; 32]) -> [u64; 4] {
    let w: Vec<u64> = state.chunks(8).map(|c| u64::from_le_bytes(c.try_into().unwrap())).collect();
    [w[0], w[1], w[2], w[3]]
}

fn bytes_of(words: [u64; 4]) -> [u8; 32] {
    let mut out = [0u8; 32];
    for (i, w) in words.iter().enumerate() {
        out[i * 8..(i + 1) * 8].copy_from_slice(&w.to_le_bytes());
    }
    out
}

fn draw(rng: &mut Xoshiro256PlusPlus, n: usize) -> Vec<u64> {
    (0..n).map(|_| rng.next_u64()).collect()
}

/// State words that exercise every lane, none all-zero.
pub fn seeds() -> Vec<[u64; 4]> {
    vec![
        [1, 2, 3, 4],
        [0x0123_4567_89ab_cdef, 0xfedc_ba98_7654_3210, 0x0f1e_2d3c_4b5a_6978, 0x8796_a5b4_c3d2_e1f0],
        [u64::MAX, 0, u64::MAX, 0],
        [0, 0, 0, 1],
    ]
}

pub const U64_SEEDS: [u64; 4] = [0, 1, 42, u64::MAX];

pub fn render(out: &mut String) {
    let streams = seeds().into_iter().map(|seed| {
        let mut rng = Xoshiro256PlusPlus::from_seed(bytes_of(seed));
        assert_eq!(words_of(rng.state()), seed, "from_seed must take the words unchanged");
        let words = draw(&mut rng, WORDS);
        let mut jumped = Xoshiro256PlusPlus::from_seed(bytes_of(seed));
        jumped.jump();
        let jumped_words = draw(&mut jumped, WORDS);
        format!(
            "\t\t{{\n{IN}seed: {},\n{IN}words: {},\n{IN}jumped: {},\n\t\t}}",
            roc::words(&seed),
            roc::u64s(&words, IN),
            roc::u64s(&jumped_words, IN)
        )
    });
    def(
        out,
        "xoshiro_streams",
        "List({ seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, words : List(U64), jumped : List(U64) })",
        &format!("[\n{},\n\t]", streams.collect::<Vec<_>>().join(",\n")),
    );

    let from_u64 = U64_SEEDS.into_iter().map(|seed| {
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(seed);
        let expanded = words_of(rng.state());
        format!(
            "\t\t{{\n{IN}seed: {},\n{IN}expanded: {},\n{IN}words: {},\n\t\t}}",
            roc::u64(seed),
            roc::words(&expanded),
            roc::u64s(&draw(&mut rng, WORDS), IN)
        )
    });
    def(
        out,
        "xoshiro_from_u64",
        "List({ seed : U64, expanded : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, words : List(U64) })",
        &format!("[\n{},\n\t]", from_u64.collect::<Vec<_>>().join(",\n")),
    );
}
