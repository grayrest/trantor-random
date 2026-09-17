//! UUIDs: the layout from RFC 9562, drawn from the `draws` sources, and
//! formatted, parsed and ordered by the `uuid` crate 1.26.0.

use uuid::Uuid;

use crate::draws::Source;
use crate::golden::Golden;
use crate::{def, roc};

const IN: &str = "\t\t\t";
const MS_MASK: u64 = (1 << 48) - 1;
const MS_LIMIT: u64 = 1 << 48;
const VARIANT: u64 = 0x8000_0000_0000_0000;

fn v4(src: &mut Source) -> Uuid {
    let hi = (src.next() & !0xF000) | 0x4000;
    let lo = (src.next() & 0x3FFF_FFFF_FFFF_FFFF) | VARIANT;
    Uuid::from_u64_pair(hi, lo)
}

fn v7(src: &mut Source, unix_ms: u64) -> Uuid {
    let rand_a = src.next() >> 52;
    let rand_b = src.next() >> 2;
    Uuid::from_u64_pair((unix_ms & MS_MASK) << 16 | 0x7000 | rand_a, VARIANT | rand_b)
}

/// `UuidV7.next` as the plan specifies it.
struct Monotonic {
    last_ms: Option<u64>,
    counter: u64,
}

impl Monotonic {
    fn next(&mut self, src: &mut Source, now: u64) -> Uuid {
        assert!(now < MS_LIMIT);
        match self.last_ms {
            Some(last) if now <= last => {
                self.counter += 1;
                if self.counter == 0x1000 {
                    self.last_ms = Some(last + 1);
                    assert!(last + 1 < MS_LIMIT);
                    self.counter = src.next() >> 53;
                }
            }
            _ => {
                self.last_ms = Some(now);
                self.counter = src.next() >> 53;
            }
        }
        let rand_b = src.next() >> 2;
        Uuid::from_u64_pair(self.last_ms.unwrap() << 16 | 0x7000 | self.counter, VARIANT | rand_b)
    }
}

fn pair(u: Uuid) -> String {
    let (hi, lo) = u.as_u64_pair();
    format!("{{ hi: {}, lo: {} }}", roc::u64(hi), roc::u64(lo))
}

fn sources(golden: &Golden) -> Vec<Source> {
    vec![Source::chacha(golden), Source::fast()]
}

pub fn render(golden: &Golden, out: &mut String) {
    // RFC 9562 Appendix A: the example v4 and v7 values, parsed by the crate.
    let examples = [("919108f7-52d1-4320-9bac-f847db4148a8", 4), ("017f22e2-79b0-7cc3-98c4-dc0c0c07398f", 7)];
    let rendered: Vec<String> = examples
        .iter()
        .map(|(text, version)| {
            let u = Uuid::parse_str(text).unwrap();
            assert_eq!(u.get_version_num(), *version);
            format!(
                "\t\t{{ text: {}, uuid: {}, version: {version}, bytes: {} }}",
                roc::str(text),
                pair(u),
                roc::bytes(u.as_bytes(), IN)
            )
        })
        .collect();
    def(
        out,
        "uuid_rfc_examples",
        "List({ text : Str, uuid : { hi : U64, lo : U64 }, version : U8, bytes : List(U8) })",
        &format!("[\n{},\n\t]", rendered.join(",\n")),
    );

    // Formatting, bytes and version over arbitrary pairs.
    let mut fast = Source::fast();
    let mut pairs: Vec<Uuid> = (0..12).map(|_| Uuid::from_u64_pair(fast.next(), fast.next())).collect();
    pairs.extend([Uuid::nil(), Uuid::max(), Uuid::from_u64_pair(0, 1), Uuid::from_u64_pair(1, 0)]);
    let rendered: Vec<String> = pairs
        .iter()
        .map(|u| {
            format!(
                "\t\t{{ uuid: {}, text: {}, version: {}, bytes: {} }}",
                pair(*u),
                roc::str(&u.hyphenated().to_string()),
                u.get_version_num(),
                roc::bytes(u.as_bytes(), IN)
            )
        })
        .collect();
    def(
        out,
        "uuid_formats",
        "List({ uuid : { hi : U64, lo : U64 }, text : Str, version : U8, bytes : List(U8) })",
        &format!("[\n{},\n\t]", rendered.join(",\n")),
    );

    // Ordering: the crate orders by bytes.
    let mut sorted = pairs.clone();
    sorted.sort();
    def(out, "uuid_sorted", "List({ hi : U64, lo : U64 })", &roc::list(sorted.into_iter().map(pair), 1, "\t"));

    // Generation. v7 times include one past 2^48 to show the mask.
    let v7_times = [0u64, 1, 0x017F_22E2_79B0, MS_MASK, MS_LIMIT + 5];
    let mut v4s = Vec::new();
    let mut v7s = Vec::new();
    for mut src in sources(golden) {
        let tag = match src {
            Source::Chacha { .. } => "Chacha",
            Source::Fast(_) => "Fast",
        };
        let generated: Vec<String> = (0..4).map(|_| pair(v4(&mut src))).collect();
        v4s.push(format!("\t\t{{ source: {tag}, uuids: {}, next: {} }}", roc::list(generated, 1, IN), roc::u64(src.next())));
        let mut src = match src {
            Source::Chacha { .. } => Source::chacha(golden),
            Source::Fast(_) => Source::fast(),
        };
        let generated: Vec<String> = v7_times.iter().map(|&ms| pair(v7(&mut src, ms))).collect();
        v7s.push(format!(
            "\t\t{{ source: {tag}, times: {}, uuids: {}, next: {} }}",
            roc::u64s(&v7_times, IN),
            roc::list(generated, 1, IN),
            roc::u64(src.next())
        ));
    }
    def(
        out,
        "uuid_v4",
        "List({ source : [Chacha, Fast], uuids : List({ hi : U64, lo : U64 }), next : U64 })",
        &format!("[\n{},\n\t]", v4s.join(",\n")),
    );
    def(
        out,
        "uuid_v7",
        "List({ source : [Chacha, Fast], times : List(U64), uuids : List({ hi : U64, lo : U64 }), next : U64 })",
        &format!("[\n{},\n\t]", v7s.join(",\n")),
    );

    // UuidV7 streams: a first call at 0, repeats, a clock going backwards.
    let short_times = [0u64, 0, 0, 5, 5, 3, 3, 9, 20, 20];
    let mut streams = Vec::new();
    for mut src in sources(golden) {
        let tag = match src {
            Source::Chacha { .. } => "Chacha",
            Source::Fast(_) => "Fast",
        };
        let mut stream = Monotonic { last_ms: None, counter: 0 };
        let generated: Vec<String> = short_times.iter().map(|&t| pair(stream.next(&mut src, t))).collect();
        streams.push(format!(
            "\t\t{{ source: {tag}, times: {}, uuids: {}, next: {} }}",
            roc::u64s(&short_times, IN),
            roc::list(generated, 1, IN),
            roc::u64(src.next())
        ));
    }
    def(
        out,
        "uuid_v7_streams",
        "List({ source : [Chacha, Fast], times : List(U64), uuids : List({ hi : U64, lo : U64 }), next : U64 })",
        &format!("[\n{},\n\t]", streams.join(",\n")),
    );

    // Counter overflow: 5000 calls in one millisecond on FastRng. Only the
    // ones around each overflow and a checksum of all are written out.
    let mut src = Source::fast();
    let mut stream = Monotonic { last_ms: None, counter: 0 };
    let all: Vec<Uuid> = (0..5000).map(|_| stream.next(&mut src, 1000)).collect();
    let checksum = all.iter().fold(0u64, |acc, u| {
        let (hi, lo) = u.as_u64_pair();
        acc.wrapping_mul(31).wrapping_add(hi).wrapping_mul(31).wrapping_add(lo)
    });
    let overflow_at: Vec<usize> = (1..all.len()).filter(|&i| all[i].as_u64_pair().0 >> 16 != all[i - 1].as_u64_pair().0 >> 16).collect();
    assert!(!overflow_at.is_empty(), "5000 calls must overflow the counter at least once");
    let around: Vec<String> = overflow_at
        .iter()
        .map(|&i| format!("\t\t\t{{ index: {i}, before: {}, after: {} }}", pair(all[i - 1]), pair(all[i])))
        .collect();
    def(
        out,
        "uuid_v7_overflow",
        "{ calls : U64, now : U64, checksum : U64, last : { hi : U64, lo : U64 }, around : List({ index : U64, before : { hi : U64, lo : U64 }, after : { hi : U64, lo : U64 } }), next : U64 }",
        &format!(
            "{{\n\t\tcalls: 5000,\n\t\tnow: 1000,\n\t\tchecksum: {},\n\t\tlast: {},\n\t\taround: [\n{},\n\t\t],\n\t\tnext: {},\n\t}}",
            roc::u64(checksum),
            pair(*all.last().unwrap()),
            around.join(",\n"),
            roc::u64(src.next())
        ),
    );
}
