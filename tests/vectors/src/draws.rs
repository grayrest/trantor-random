//! A model of every draw, written from the plan's algorithm section
//! (trantor `plans/2026-09-16-trantor-random.md`), not from the Roc code.
//!
//! Each case runs on two word sources: the Go ChaCha8 stream for chacha seed 1
//! and rand_xoshiro for xoshiro seed 1. After each case the next word the
//! source would give is recorded, so a draw that consumes one word too many or
//! too few fails.

use rand_xoshiro::Xoshiro256PlusPlus;
use rand_xoshiro::rand_core::{Rng, SeedableRng};

use crate::golden::Golden;
use crate::{def, roc, xoshiro};

const IN: &str = "\t\t\t";

pub enum Source {
    Chacha { words: Vec<u64>, at: usize },
    Fast(Xoshiro256PlusPlus),
}

impl Source {
    pub fn chacha(golden: &Golden) -> Source {
        Source::Chacha { words: golden.streams[1].words.clone(), at: 0 }
    }

    pub fn fast() -> Source {
        let seed = xoshiro::seeds()[1];
        let mut bytes = [0u8; 32];
        for (i, w) in seed.iter().enumerate() {
            bytes[i * 8..(i + 1) * 8].copy_from_slice(&w.to_le_bytes());
        }
        Source::Fast(Xoshiro256PlusPlus::from_seed(bytes))
    }

    fn tag(&self) -> &'static str {
        match self {
            Source::Chacha { .. } => "Chacha",
            Source::Fast(_) => "Fast",
        }
    }

    pub fn next(&mut self) -> u64 {
        match self {
            Source::Chacha { words, at } => {
                let word = *words.get(*at).expect("case drew past the 372 golden words");
                *at += 1;
                word
            }
            Source::Fast(rng) => rng.next_u64(),
        }
    }

    pub fn u32(&mut self) -> u32 {
        (self.next() >> 32) as u32
    }

    pub fn u8(&mut self) -> u8 {
        (self.next() >> 56) as u8
    }

    pub fn bool(&mut self) -> bool {
        self.next() >> 63 == 1
    }

    /// Lemire's nearly-divisionless method.
    pub fn below(&mut self, n: u64) -> u64 {
        assert!(n != 0);
        let mut m = self.next() as u128 * n as u128;
        if (m as u64) < n {
            let threshold = n.wrapping_neg() % n;
            while (m as u64) < threshold {
                m = self.next() as u128 * n as u128;
            }
        }
        (m >> 64) as u64
    }

    pub fn between_u64(&mut self, a: u64, b: u64) -> u64 {
        let (lo, hi) = if a > b { (b, a) } else { (a, b) };
        let span = hi - lo;
        if span == u64::MAX { self.next() } else { lo + self.below(span + 1) }
    }

    pub fn between_i64(&mut self, a: i64, b: i64) -> i64 {
        const FLIP: u64 = 1 << 63;
        let u = self.between_u64(a as u64 ^ FLIP, b as u64 ^ FLIP);
        (u ^ FLIP) as i64
    }

    pub fn f64(&mut self) -> f64 {
        (self.next() >> 11) as f64 * (1.0 / (1u64 << 53) as f64)
    }

    pub fn between_f64(&mut self, a: f64, b: f64) -> f64 {
        let (lo, hi) = if a > b { (b, a) } else { (a, b) };
        if lo == hi {
            return lo;
        }
        let span = hi - lo;
        assert!(span.is_finite());
        loop {
            let x = lo + self.f64() * span;
            if x < hi {
                return x;
            }
        }
    }

    pub fn chance(&mut self, p: f64) -> bool {
        self.f64() < p
    }

    pub fn bytes(&mut self, n: usize) -> Vec<u8> {
        let mut out = Vec::new();
        while out.len() < n {
            out.extend(self.next().to_le_bytes());
        }
        out.truncate(n);
        out
    }

    pub fn shuffle(&mut self, mut list: Vec<u64>) -> Vec<u64> {
        for i in (1..list.len()).rev() {
            let j = self.below(i as u64 + 1) as usize;
            list.swap(i, j);
        }
        list
    }

    pub fn choose(&mut self, list: &[u64]) -> Option<u64> {
        if list.is_empty() { None } else { Some(list[self.below(list.len() as u64) as usize]) }
    }

    pub fn sample(&mut self, mut list: Vec<u64>, k: usize) -> Vec<u64> {
        let take = k.min(list.len());
        for i in 0..take {
            let j = i + self.below((list.len() - i) as u64) as usize;
            list.swap(i, j);
        }
        list.truncate(take);
        list
    }

    /// `Weights.from_list` then `pick`: the first entry whose cumulative
    /// weight exceeds `below(total)`.
    pub fn pick(&mut self, weights: &[u64]) -> usize {
        let cumulative: Vec<u64> = weights
            .iter()
            .scan(0u64, |sum, w| {
                *sum += w;
                Some(*sum)
            })
            .collect();
        let x = self.below(*cumulative.last().unwrap());
        cumulative.partition_point(|&c| c <= x)
    }
}

fn items(len: usize) -> Vec<u64> {
    (0..len as u64).map(|i| i * 10 + 1).collect()
}

/// Every source for a case, in the order the Roc checks expect.
fn sources(golden: &Golden) -> Vec<Source> {
    vec![Source::chacha(golden), Source::fast()]
}

struct Case {
    fields: String,
}

fn cases(out: &mut String, name: &str, fields_type: &str, golden: &Golden, build: impl Fn(&mut Source) -> Vec<Case>) {
    let mut rendered = Vec::new();
    for mut source in sources(golden) {
        let tag = source.tag();
        for case in build(&mut source) {
            rendered.push(format!("\t\t{{ source: {tag}, {} }}", case.fields));
        }
    }
    def(
        out,
        name,
        &format!("List({{ source : [Chacha, Fast], {fields_type} }})"),
        &format!("[\n{},\n\t]", rendered.join(",\n")),
    );
}

/// Runs `op` on a fresh source per argument set and records its results and
/// the word that follows.
fn per_args<A: Clone>(source_tag: &Source, golden: &Golden, args: &[A], op: impl Fn(&mut Source, A) -> String) -> Vec<Case> {
    args.iter()
        .map(|a| {
            let mut fresh = match source_tag {
                Source::Chacha { .. } => Source::chacha(golden),
                Source::Fast(_) => Source::fast(),
            };
            let fields = op(&mut fresh, a.clone());
            let next = fresh.next();
            Case { fields: format!("{fields}, next: {}", roc::u64(next)) }
        })
        .collect()
}

const REPEAT: usize = 6;

pub fn render(golden: &Golden, out: &mut String) {
    cases(out, "draw_u64", "results : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &[()], |src, ()| format!("results: {}", roc::u64s(&(0..REPEAT).map(|_| src.next()).collect::<Vec<_>>(), IN)))
    });
    cases(out, "draw_u32", "results : List(U32), next : U64", golden, |s| {
        per_args(s, golden, &[()], |src, ()| {
            format!("results: {}", roc::list((0..REPEAT).map(|_| src.u32().to_string()), 8, IN))
        })
    });
    cases(out, "draw_u8", "results : List(U8), next : U64", golden, |s| {
        per_args(s, golden, &[()], |src, ()| format!("results: {}", roc::list((0..REPEAT).map(|_| src.u8().to_string()), 8, IN)))
    });
    cases(out, "draw_bool", "results : List(Bool), next : U64", golden, |s| {
        per_args(s, golden, &[()], |src, ()| format!("results: {}", roc::list((0..REPEAT).map(|_| roc::bool(src.bool())), 8, IN)))
    });

    let below_args = [1u64, 2, 3, 6, 1000, 1 << 63, (1 << 63) + 1, u64::MAX - 1, u64::MAX];
    cases(out, "draw_below", "n : U64, results : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &below_args, |src, n| {
            format!("n: {}, results: {}", roc::u64(n), roc::u64s(&(0..REPEAT).map(|_| src.below(n)).collect::<Vec<_>>(), IN))
        })
    });

    let between_u64_args = [(1u64, 6u64), (6, 1), (5, 5), (0, u64::MAX), (u64::MAX, 0), (10, u64::MAX), (0, 1 << 63)];
    cases(out, "draw_between_u64", "lo : U64, hi : U64, results : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &between_u64_args, |src, (lo, hi)| {
            let results: Vec<u64> = (0..REPEAT).map(|_| src.between_u64(lo, hi)).collect();
            format!("lo: {}, hi: {}, results: {}", roc::u64(lo), roc::u64(hi), roc::u64s(&results, IN))
        })
    });

    let between_i64_args = [(-3i64, 3i64), (3, -3), (i64::MIN, i64::MAX), (i64::MAX, i64::MIN), (i64::MIN, 0), (-1, -1), (-100, 100)];
    cases(out, "draw_between_i64", "lo : I64, hi : I64, results : List(I64), next : U64", golden, |s| {
        per_args(s, golden, &between_i64_args, |src, (lo, hi)| {
            let results = (0..REPEAT).map(|_| roc::i64(src.between_i64(lo, hi)));
            format!("lo: {}, hi: {}, results: {}", roc::i64(lo), roc::i64(hi), roc::list(results, 4, IN))
        })
    });

    cases(out, "draw_f64", "results : List(F64), next : U64", golden, |s| {
        per_args(s, golden, &[()], |src, ()| format!("results: {}", roc::list((0..REPEAT).map(|_| roc::f64(src.f64())), 2, IN)))
    });

    let between_f64_args = [(1.0f64, 3.0f64), (3.0, 1.0), (-1e308, 1e307), (0.5, 0.5), (-2.5, -2.25)];
    cases(out, "draw_between_f64", "lo : F64, hi : F64, results : List(F64), next : U64", golden, |s| {
        per_args(s, golden, &between_f64_args, |src, (lo, hi)| {
            let results = (0..REPEAT).map(|_| roc::f64(src.between_f64(lo, hi)));
            format!("lo: {}, hi: {}, results: {}", roc::f64(lo), roc::f64(hi), roc::list(results, 2, IN))
        })
    });

    let chance_args = [0.0f64, 0.25, 0.5, 1.0];
    cases(out, "draw_chance", "p : F64, results : List(Bool), next : U64", golden, |s| {
        per_args(s, golden, &chance_args, |src, p| {
            format!("p: {}, results: {}", roc::f64(p), roc::list((0..REPEAT).map(|_| roc::bool(src.chance(p))), 8, IN))
        })
    });

    let bytes_args = [0usize, 1, 7, 8, 9, 20];
    cases(out, "draw_bytes", "n : U64, result : List(U8), next : U64", golden, |s| {
        per_args(s, golden, &bytes_args, |src, n| format!("n: {n}, result: {}", roc::bytes(&src.bytes(n), IN)))
    });

    let shuffle_args = [0usize, 1, 2, 5, 20];
    cases(out, "draw_shuffle", "len : U64, result : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &shuffle_args, |src, len| format!("len: {len}, result: {}", roc::u64s(&src.shuffle(items(len)), IN)))
    });

    let choose_args = [0usize, 1, 7];
    cases(out, "draw_choose", "len : U64, result : Try(U64, [ListWasEmpty]), next : U64", golden, |s| {
        per_args(s, golden, &choose_args, |src, len| {
            let result = match src.choose(&items(len)) {
                Some(item) => format!("Ok({})", roc::u64(item)),
                None => "Err(ListWasEmpty)".to_string(),
            };
            format!("len: {len}, result: {result}")
        })
    });

    let sample_args = [(0usize, 0usize), (5, 0), (5, 1), (5, 3), (5, 5), (5, 8), (20, 6), (1, 1)];
    cases(out, "draw_sample", "len : U64, k : U64, result : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &sample_args, |src, (len, k)| {
            format!("len: {len}, k: {k}, result: {}", roc::u64s(&src.sample(items(len), k), IN))
        })
    });

    let pick_args: Vec<Vec<u64>> = vec![vec![1], vec![0, 5, 0, 5, 0], vec![1, 2, 3, 4], vec![u64::MAX - 1, 1], vec![0, 0, 7]];
    cases(out, "draw_pick", "weights : List(U64), results : List(U64), next : U64", golden, |s| {
        per_args(s, golden, &pick_args, |src, weights| {
            let results: Vec<u64> = (0..REPEAT).map(|_| src.pick(&weights) as u64).collect();
            format!("weights: {}, results: {}", roc::u64s(&weights, IN), roc::u64s(&results, IN))
        })
    });
}
