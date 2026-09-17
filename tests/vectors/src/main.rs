//! Regenerates `components/random/RngVectors.roc` (and, with `--regen-go`,
//! `go/golden.txt` first).

use std::process::Command;

fn main() {
    if std::env::args().any(|arg| arg == "--regen-go") {
        let output = Command::new("go")
            .arg("run")
            .arg(".")
            .current_dir(trantor_random_vectors::go_dir())
            .output()
            .expect("run `go run .` in tests/vectors/go (is Go on PATH?)");
        assert!(output.status.success(), "go run failed:\n{}", String::from_utf8_lossy(&output.stderr));
        std::fs::write(trantor_random_vectors::golden_path(), output.stdout).expect("write go/golden.txt");
    }
    std::fs::write(trantor_random_vectors::vectors_path(), trantor_random_vectors::render()).expect("write RngVectors.roc");
}
