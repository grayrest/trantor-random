import IOErr exposing [IOErr]

## Stands in for trantor-cli's `Random` so the generators build on the roc
## repo's alloc-count platform. Never called by the gate.
Random :: [].{
	seed_u64! : () => Try(U64, [RandomErr(IOErr), ..])
	seed_u64! = || Ok(0)
}
