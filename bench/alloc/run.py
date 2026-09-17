#!/usr/bin/env python3
"""The D-S4-12 allocation and speed gate, outside `trantor test`.

trantor-cli has no allocation counter, so the generators are built against the
roc repo's `test/alloc-count` platform (`Host.alloc_count!`), next to stand-ins
for trantor-cli's `Random` and `IOErr`. Needs `roc` on PATH and a roc checkout
(`ROC_REPO`, default ~/Repositories/roc).

Prints allocation counts, then ns per u64 for each generator: each build is
run REPS times at SCALE and at scale 0, and the fastest of each is taken.
"""

import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
COMPONENT = HERE.parent.parent / "components" / "random"
ROC_REPO = Path(os.environ.get("ROC_REPO", Path.home() / "Repositories" / "roc"))
PLATFORM = ROC_REPO / "test" / "alloc-count" / "platform" / "main.roc"
HOST_INPUT_BYTES = 16
SCALE = 625_000  # 16 x 625_000 = 10M draws per timed run
REPS = 5
BODIES = {
    "alloc": "alloc_report!(bytes, count)",
    "rng": "rng_sum(Rng.from_u64(bytes), count, 0).to_str()",
    "fast": "fast_sum(FastRng.from_u64(bytes), count, 0).to_str()",
}


def build(work: Path, mode: str, scale: int) -> Path:
    source = (HERE / "main.roc.in").read_text()
    source = source.replace("@PLATFORM@", str(PLATFORM)).replace("@BODY@", BODIES[mode]).replace("@SCALE@", str(scale))
    (work / "main.roc").write_text(source)
    out = work / f"gate-{mode}-{scale}"
    subprocess.run(["roc", "build", "--opt=speed", f"--output={out}", str(work / "main.roc")], check=True, cwd=work)
    return out


def fastest(binary: Path) -> float:
    best = float("inf")
    for _ in range(REPS):
        start = time.perf_counter()
        subprocess.run([str(binary)], check=True, capture_output=True)
        best = min(best, time.perf_counter() - start)
    return best


def main() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp)
        for module in COMPONENT.glob("*.roc"):
            shutil.copy(module, work / module.name)
        for stand_in in ("Random.roc", "IOErr.roc"):
            shutil.copy(HERE / stand_in, work / stand_in)

        alloc = build(work, "alloc", 62_500)
        print(subprocess.run([str(alloc)], check=True, capture_output=True, text=True).stderr.strip())

        draws = HOST_INPUT_BYTES * SCALE
        ns = {}
        for mode in ("rng", "fast"):
            loaded = fastest(build(work, mode, SCALE))
            empty = fastest(build(work, mode, 0))
            ns[mode] = (loaded - empty) * 1e9 / draws
            print(f"{mode}: {ns[mode]:.2f} ns per u64 over {draws} draws")
        print(f"Rng / FastRng: {ns['rng'] / ns['fast']:.1f}x")
    return 0


if __name__ == "__main__":
    sys.exit(main())
