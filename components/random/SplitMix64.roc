import RngVectors

## SplitMix64, used only to expand a `U64` test seed into four words, as the
## xoshiro authors recommend. Not part of the package surface.
SplitMix64 :: [].{

	## The first four outputs from state `seed`.
	expand : U64 -> { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }
	expand = |seed| {
		a = next(seed)
		b = next(a.state)
		c = next(b.state)
		d = next(c.state)
		{ w0: a.value, w1: b.value, w2: c.value, w3: d.value }
	}

	next : U64 -> { value : U64, state : U64 }
	next = |state| {
		advanced = state.plus_wrap(0x9e3779b97f4a7c15)
		z1 = advanced.bitwise_xor(advanced.shr_zf_wrap(30)).times_wrap(0xbf58476d1ce4e5b9)
		z2 = z1.bitwise_xor(z1.shr_zf_wrap(27)).times_wrap(0x94d049bb133111eb)
		{ value: z2.bitwise_xor(z2.shr_zf_wrap(31)), state: advanced }
	}
}

expect
	RngVectors.xoshiro_from_u64.all(|v| SplitMix64.expand(v.seed) == v.expanded)
