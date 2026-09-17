//! Go's ChaCha8 reference outputs, parsed from `go/golden.txt`.

use crate::{def, roc};

pub struct Stream {
    pub seed: [u8; 32],
    pub words: Vec<u64>,
}

pub struct Fork {
    pub seed: [u8; 32],
    pub skip: u64,
    pub child: Vec<u64>,
    pub parent: Vec<u64>,
}

pub struct State {
    pub seed: [u8; 32],
    pub draws: u64,
    pub state: Vec<u8>,
    pub next: Vec<u64>,
}

pub struct Unmarshal {
    pub state: Vec<u8>,
    pub ok: bool,
}

pub struct Golden {
    pub streams: Vec<Stream>,
    pub forks: Vec<Fork>,
    pub states: Vec<State>,
    pub unmarshals: Vec<Unmarshal>,
}

pub fn read() -> Golden {
    let text = std::fs::read_to_string(crate::golden_path()).expect("read go/golden.txt");
    let mut golden = Golden { streams: vec![], forks: vec![], states: vec![], unmarshals: vec![] };
    for line in text.lines().filter(|l| !l.starts_with('#') && !l.is_empty()) {
        let fields: Vec<&str> = line.split_whitespace().collect();
        match fields[0] {
            "stream" => golden.streams.push(Stream { seed: seed(fields[1]), words: words(&fields[2..]) }),
            "fork" => {
                let child_at = fields.iter().position(|f| *f == "child").expect("child");
                let parent_at = fields.iter().position(|f| *f == "parent").expect("parent");
                golden.forks.push(Fork {
                    seed: seed(fields[1]),
                    skip: fields[2].parse().expect("skip"),
                    child: words(&fields[child_at + 1..parent_at]),
                    parent: words(&fields[parent_at + 1..]),
                });
            }
            "marshal" => golden.states.push(State {
                seed: seed(fields[1]),
                draws: fields[2].parse().expect("draws"),
                state: roc::from_hex(fields[3]),
                next: words(&fields[4..]),
            }),
            "unmarshal" => golden.unmarshals.push(Unmarshal { state: roc::from_hex(fields[1]), ok: fields[2] == "true" }),
            other => panic!("golden.txt: unknown line kind {other}"),
        }
    }
    golden
}

fn seed(hex: &str) -> [u8; 32] {
    roc::from_hex(hex).try_into().expect("32-byte seed")
}

fn words(fields: &[&str]) -> Vec<u64> {
    fields.iter().map(|w| u64::from_str_radix(w, 16).expect("hex word")).collect()
}

pub fn render(golden: &Golden, out: &mut String) {
    const IN: &str = "\t\t\t";
    let streams = golden.streams.iter().map(|s| {
        format!("\t\t{{\n{IN}seed: {},\n{IN}words: {},\n\t\t}}", roc::seed(&s.seed), roc::u64s(&s.words, IN))
    });
    def(
        out,
        "chacha_streams",
        "List({ seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, words : List(U64) })",
        &format!("[\n{},\n\t]", streams.collect::<Vec<_>>().join(",\n")),
    );

    let forks = golden.forks.iter().map(|f| {
        format!(
            "\t\t{{\n{IN}seed: {},\n{IN}skip: {},\n{IN}child: {},\n{IN}parent: {},\n\t\t}}",
            roc::seed(&f.seed),
            f.skip,
            roc::u64s(&f.child, IN),
            roc::u64s(&f.parent, IN)
        )
    });
    def(
        out,
        "chacha_forks",
        "List({ seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, skip : U64, child : List(U64), parent : List(U64) })",
        &format!("[\n{},\n\t]", forks.collect::<Vec<_>>().join(",\n")),
    );

    let states = golden.states.iter().map(|s| {
        format!(
            "\t\t{{\n{IN}seed: {},\n{IN}draws: {},\n{IN}state: {},\n{IN}next: {},\n\t\t}}",
            roc::seed(&s.seed),
            s.draws,
            roc::bytes(&s.state, IN),
            roc::u64s(&s.next, IN)
        )
    });
    def(
        out,
        "chacha_states",
        "List({ seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, draws : U64, state : List(U8), next : List(U64) })",
        &format!("[\n{},\n\t]", states.collect::<Vec<_>>().join(",\n")),
    );

    let unmarshals = golden.unmarshals.iter().map(|u| {
        format!("\t\t{{\n{IN}state: {},\n{IN}ok: {},\n\t\t}}", roc::bytes(&u.state, IN), roc::bool(u.ok))
    });
    def(
        out,
        "chacha_unmarshal",
        "List({ state : List(U8), ok : Bool })",
        &format!("[\n{},\n\t]", unmarshals.collect::<Vec<_>>().join(",\n")),
    );
}
