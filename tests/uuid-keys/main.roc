app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr exposing [OsStr]
import pf.Stdout
import pf.Uuid

## A `Uuid` keyed from outside the module that defines it, which is the only
## place the `Dict` key claim in README.md can be checked. An in-module
## `expect` cannot reach it: inside `Uuid.roc` a `to_hash` that inferred a
## structural type still unifies with the `Uuid` it is handed, so the mismatch
## only surfaces where a consumer names the nominal type.

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	a = Uuid.from_str("017f22e2-79b0-7cc3-98c4-dc0c0c07398f")?
	b = Uuid.from_str("919108f7-52d1-4320-9bac-f847db4148a8")?

	table = Dict.empty().insert(a, "first").insert(b, "second")
	Stdout.line!("dict key: ${Str.inspect(table.get(a) == Ok("first") and table.get(b) == Ok("second"))}")?
	Stdout.line!("dict miss: ${Str.inspect(table.get(Uuid.nil) == Err(KeyNotFound))}")?

	seen = Set.empty().insert(a).insert(b)
	Stdout.line!("set member: ${Str.inspect(seen.contains(a) and seen.contains(b))}")?
	Stdout.line!("set non-member: ${Str.inspect(!seen.contains(Uuid.max))}")
}
