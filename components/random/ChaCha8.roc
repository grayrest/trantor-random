## The ChaCha8 block function as C2SP chacha8rand uses it, ported from Go's
## `internal/chacha8rand/chacha8_generic.go` (`block_generic`, `setup`, `qr`).
##
## Not part of the package surface: `Rng` is.
ChaCha8 :: [].{

	Words : { x0 : U32, x1 : U32, x2 : U32, x3 : U32, x4 : U32, x5 : U32, x6 : U32, x7 : U32, x8 : U32, x9 : U32, x10 : U32, x11 : U32, x12 : U32, x13 : U32, x14 : U32, x15 : U32 }

	## Four ChaCha8 blocks under `seed`, counters `counter` to `counter + 3`,
	## written into the 32 words of `buf`.
	##
	## Go reads its `[32]uint64` buffer as `[16][4]uint32`: row `r` of the
	## matrix, lane `i` for the block with counter `counter + i`. On a
	## little-endian machine that makes word `2r` lanes 0 and 1 of row `r`, low
	## half first, and word `2r + 1` lanes 2 and 3.
	fill : List(U64), { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, U32 -> List(U64)
	fill = |buf, seed, counter| {
		b0 = block(seed, counter)
		b1 = block(seed, counter.plus_wrap(1))
		b2 = block(seed, counter.plus_wrap(2))
		b3 = block(seed, counter.plus_wrap(3))
		var $buf = buf
		$buf = put($buf, 0, join(b0.x0, b1.x0))
		$buf = put($buf, 1, join(b2.x0, b3.x0))
		$buf = put($buf, 2, join(b0.x1, b1.x1))
		$buf = put($buf, 3, join(b2.x1, b3.x1))
		$buf = put($buf, 4, join(b0.x2, b1.x2))
		$buf = put($buf, 5, join(b2.x2, b3.x2))
		$buf = put($buf, 6, join(b0.x3, b1.x3))
		$buf = put($buf, 7, join(b2.x3, b3.x3))
		$buf = put($buf, 8, join(b0.x4, b1.x4))
		$buf = put($buf, 9, join(b2.x4, b3.x4))
		$buf = put($buf, 10, join(b0.x5, b1.x5))
		$buf = put($buf, 11, join(b2.x5, b3.x5))
		$buf = put($buf, 12, join(b0.x6, b1.x6))
		$buf = put($buf, 13, join(b2.x6, b3.x6))
		$buf = put($buf, 14, join(b0.x7, b1.x7))
		$buf = put($buf, 15, join(b2.x7, b3.x7))
		$buf = put($buf, 16, join(b0.x8, b1.x8))
		$buf = put($buf, 17, join(b2.x8, b3.x8))
		$buf = put($buf, 18, join(b0.x9, b1.x9))
		$buf = put($buf, 19, join(b2.x9, b3.x9))
		$buf = put($buf, 20, join(b0.x10, b1.x10))
		$buf = put($buf, 21, join(b2.x10, b3.x10))
		$buf = put($buf, 22, join(b0.x11, b1.x11))
		$buf = put($buf, 23, join(b2.x11, b3.x11))
		$buf = put($buf, 24, join(b0.x12, b1.x12))
		$buf = put($buf, 25, join(b2.x12, b3.x12))
		$buf = put($buf, 26, join(b0.x13, b1.x13))
		$buf = put($buf, 27, join(b2.x13, b3.x13))
		$buf = put($buf, 28, join(b0.x14, b1.x14))
		$buf = put($buf, 29, join(b2.x14, b3.x14))
		$buf = put($buf, 30, join(b0.x15, b1.x15))
		$buf = put($buf, 31, join(b2.x15, b3.x15))
		$buf
	}

	## `list` with `value` at `index`, which is always in range here. The error
	## branch does not mention `list`, so a unique list is updated in place.
	put : List(U64), U64, U64 -> List(U64)
	put = |list, index, value|
		match list.set(index, value) {
			Ok(updated) => updated
			Err(_) => crash "ChaCha8.put: index ${index.to_str()} outside the 32-word buffer"
		}

	join : U32, U32 -> U64
	join = |low, high| low.to_u64().bitwise_or(high.to_u64().shl_wrap(32))

	## One block (`setup` and the loop body of `block_generic`). The constants
	## and the counter are not added back at the end, and the seed words are.
	block : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }, U32 -> Words
	block = |seed, counter| {
		k4 = seed.w0.to_u32_wrap()
		k5 = seed.w0.shr_zf_wrap(32).to_u32_wrap()
		k6 = seed.w1.to_u32_wrap()
		k7 = seed.w1.shr_zf_wrap(32).to_u32_wrap()
		k8 = seed.w2.to_u32_wrap()
		k9 = seed.w2.shr_zf_wrap(32).to_u32_wrap()
		k10 = seed.w3.to_u32_wrap()
		k11 = seed.w3.shr_zf_wrap(32).to_u32_wrap()
		start = {
			x0: 0x61707865, x1: 0x3320646e, x2: 0x79622d32, x3: 0x6b206574,
			x4: k4, x5: k5, x6: k6, x7: k7, x8: k8, x9: k9, x10: k10, x11: k11,
			x12: counter, x13: 0, x14: 0, x15: 0,
		}
		s = double_round(double_round(double_round(double_round(start))))
		{ ..s,
			x4: s.x4.plus_wrap(k4), x5: s.x5.plus_wrap(k5), x6: s.x6.plus_wrap(k6), x7: s.x7.plus_wrap(k7),
			x8: s.x8.plus_wrap(k8), x9: s.x9.plus_wrap(k9), x10: s.x10.plus_wrap(k10), x11: s.x11.plus_wrap(k11),
		}
	}

	## Eight quarter rounds: the columns, then the diagonals.
	double_round : Words -> Words
	double_round = |s| {
		q0 = qr(s.x0, s.x4, s.x8, s.x12)
		q1 = qr(s.x1, s.x5, s.x9, s.x13)
		q2 = qr(s.x2, s.x6, s.x10, s.x14)
		q3 = qr(s.x3, s.x7, s.x11, s.x15)
		q4 = qr(q0.a, q1.b, q2.c, q3.d)
		q5 = qr(q1.a, q2.b, q3.c, q0.d)
		q6 = qr(q2.a, q3.b, q0.c, q1.d)
		q7 = qr(q3.a, q0.b, q1.c, q2.d)
		{ x0: q4.a, x1: q5.a, x2: q6.a, x3: q7.a, x4: q7.b, x5: q4.b, x6: q5.b, x7: q6.b, x8: q6.c, x9: q7.c, x10: q4.c, x11: q5.c, x12: q5.d, x13: q6.d, x14: q7.d, x15: q4.d }
	}

	qr : U32, U32, U32, U32 -> { a : U32, b : U32, c : U32, d : U32 }
	qr = |a0, b0, c0, d0| {
		a1 = a0.plus_wrap(b0)
		d1 = rotl(d0.bitwise_xor(a1), 16)
		c1 = c0.plus_wrap(d1)
		b1 = rotl(b0.bitwise_xor(c1), 12)
		a2 = a1.plus_wrap(b1)
		d2 = rotl(d1.bitwise_xor(a2), 8)
		c2 = c1.plus_wrap(d2)
		b2 = rotl(b1.bitwise_xor(c2), 7)
		{ a: a2, b: b2, c: c2, d: d2 }
	}

	rotl : U32, U8 -> U32
	rotl = |x, n| x.shl_wrap(n).bitwise_or(x.shr_zf_wrap(32 - n))
}
