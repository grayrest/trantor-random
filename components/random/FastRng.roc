import IOErr exposing [IOErr]
import Random
import SplitMix64
import RngDraw
import Weights
import RngVectors

## A fast random number generator: xoshiro256++.
##
## Use it for simulations, games and tests that want many cheap draws. Its
## output is predictable: anyone who sees four draws can compute every later
## one, so use `Rng` for UUIDs, tokens and anything else someone might want to
## guess.
##
## Draws work as on `Rng`: each returns the value and the generator to draw
## from next. For a given seed the sequence is the same in every 1.x release.
FastRng :: { s0 : U64, s1 : U64, s2 : U64, s3 : U64 }.{

	## A generator seeded with 256 bits from the operating system.
	from_os! : () => Try(FastRng, [RandomErr(IOErr), ..])
	from_os! = || {
		w0 = Random.seed_u64!()?
		w1 = Random.seed_u64!()?
		w2 = Random.seed_u64!()?
		w3 = Random.seed_u64!()?
		Ok(from_words({ w0, w1, w2, w3 }) ?? from_u64(w0))
	}

	## A generator whose state is exactly these four words. xoshiro cannot
	## leave the all-zero state, so that one is refused.
	from_words : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 } -> Try(FastRng, [AllZero, ..])
	from_words = |{ w0, w1, w2, w3 }|
		if w0 == 0 and w1 == 0 and w2 == 0 and w3 == 0 {
			Err(AllZero)
		} else {
			Ok(FastRng.{ s0: w0, s1: w1, s2: w2, s3: w3 })
		}

	## A generator from a `U64`, expanded to 256 bits with SplitMix64 as the
	## xoshiro authors recommend. It carries 64 bits of entropy at most.
	from_u64 : U64 -> FastRng
	from_u64 = |seed| {
		{ w0, w1, w2, w3 } = SplitMix64.expand(seed)
		FastRng.{ s0: w0, s1: w1, s2: w2, s3: w3 }
	}

	## The next 64 random bits.
	next_u64 : FastRng -> (U64, FastRng)
	next_u64 = |rng| {
		result = rotl(rng.s0.plus_wrap(rng.s3), 23).plus_wrap(rng.s0)
		t = rng.s1.shl_wrap(17)
		s2 = rng.s2.bitwise_xor(rng.s0)
		s3 = rng.s3.bitwise_xor(rng.s1)
		s1 = rng.s1.bitwise_xor(s2)
		s0 = rng.s0.bitwise_xor(s3)
		(result, FastRng.{ s0, s1, s2: s2.bitwise_xor(t), s3: rotl(s3, 45) })
	}

	# Draws: each forwards to `RngDraw`, where they are documented in full.

	## 64 random bits.
	u64 : FastRng -> (U64, FastRng)
	u64 = |rng| RngDraw.u64(rng)

	## 32 random bits.
	u32 : FastRng -> (U32, FastRng)
	u32 = |rng| RngDraw.u32(rng)

	## 8 random bits.
	u8 : FastRng -> (U8, FastRng)
	u8 = |rng| RngDraw.u8(rng)

	## `True` or `False`.
	bool : FastRng -> (Bool, FastRng)
	bool = |rng| RngDraw.bool(rng)

	## A number in `[0, n)`; `n` of 0 crashes.
	below : FastRng, U64 -> (U64, FastRng)
	below = |rng, n| RngDraw.below(rng, n)

	## A number in `[lo, hi]`; reversed bounds are swapped.
	between_u64 : FastRng, U64, U64 -> (U64, FastRng)
	between_u64 = |rng, lo, hi| RngDraw.between_u64(rng, lo, hi)

	## A number in `[lo, hi]`; reversed bounds are swapped.
	between_i64 : FastRng, I64, I64 -> (I64, FastRng)
	between_i64 = |rng, lo, hi| RngDraw.between_i64(rng, lo, hi)

	## A float in `[0, 1)`.
	f64 : FastRng -> (F64, FastRng)
	f64 = |rng| RngDraw.f64(rng)

	## A float in `[lo, hi)`; see `RngDraw.between_f64`.
	between_f64 : FastRng, F64, F64 -> (F64, FastRng)
	between_f64 = |rng, lo, hi| RngDraw.between_f64(rng, lo, hi)

	## `True` with probability `p`.
	chance : FastRng, F64 -> (Bool, FastRng)
	chance = |rng, p| RngDraw.chance(rng, p)

	## `n` random bytes.
	bytes : FastRng, U64 -> (List(U8), FastRng)
	bytes = |rng, n| RngDraw.bytes(rng, n)

	## `list` in random order.
	shuffle : FastRng, List(a) -> (List(a), FastRng)
	shuffle = |rng, list| RngDraw.shuffle(rng, list)

	## One item of `list`, each equally likely.
	choose : FastRng, List(a) -> (Try(a, [ListWasEmpty, ..]), FastRng)
	choose = |rng, list| RngDraw.choose(rng, list)

	## `k` different items of `list`, in the order drawn.
	sample : FastRng, List(a), U64 -> (List(a), FastRng)
	sample = |rng, list, k| RngDraw.sample(rng, list, k)

	## An item of `weights`, each in proportion to its weight.
	pick : FastRng, Weights(a) -> (a, FastRng)
	pick = |rng, weights| RngDraw.pick(rng, weights)

	## `(child, parent)`: the child continues this generator's sequence and the
	## parent moves 2^128 draws ahead, so the two cannot overlap for any
	## practical length. Forking the parent again gives a different child, so
	## `(c1, p1) = rng.fork()` then `(c2, p2) = p1.fork()` hands out distinct
	## streams, as it does on `Rng`.
	fork : FastRng -> (FastRng, FastRng)
	fork = |rng| (rng, jump(rng))

	## This generator's state, as 32 bytes that `from_bytes` turns back into a
	## generator continuing the same sequence. Bytes saved by any 1.x release
	## load in any later 1.x release.
	to_bytes : FastRng -> List(U8)
	to_bytes = |rng| [rng.s0, rng.s1, rng.s2, rng.s3].join_map(le_bytes)

	## The generator `to_bytes` saved. Anything but 32 bytes, or the all-zero
	## state no generator reaches, is refused.
	from_bytes : List(U8) -> Try(FastRng, [InvalidState, ..])
	from_bytes = |data|
		if data.len() != 32 {
			Err(InvalidState)
		} else {
			words = {
				w0: U64.from_le_bytes(data, 0) ?? 0,
				w1: U64.from_le_bytes(data, 8) ?? 0,
				w2: U64.from_le_bytes(data, 16) ?? 0,
				w3: U64.from_le_bytes(data, 24) ?? 0,
			}
			from_words(words).map_err(|AllZero| InvalidState)
		}

	le_bytes : U64 -> List(U8)
	le_bytes = |word| [0, 8, 16, 24, 32, 40, 48, 56].map(|shift| word.shr_zf_wrap(shift).to_u8_wrap())

	## The generator after 2^128 draws (the reference `jump`).
	jump : FastRng -> FastRng
	jump = |rng| {
		start = { acc: { s0: 0, s1: 0, s2: 0, s3: 0 }, rng }
		done = [0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c].fold(start, |state, constant| jump_word(state, constant, 0))
		FastRng.{ s0: done.acc.s0, s1: done.acc.s1, s2: done.acc.s2, s3: done.acc.s3 }
	}

	## For each bit of `constant`, least significant first: when it is set, XOR
	## the current state into the accumulator; then step the generator.
	jump_word : { acc : { s0 : U64, s1 : U64, s2 : U64, s3 : U64 }, rng : FastRng }, U64, U8 -> { acc : { s0 : U64, s1 : U64, s2 : U64, s3 : U64 }, rng : FastRng }
	jump_word = |{ acc, rng }, constant, bit|
		if bit == 64 {
			{ acc, rng }
		} else {
			next_acc =
				if constant.shr_zf_wrap(bit).bitwise_and(1) == 1 {
					{ s0: acc.s0.bitwise_xor(rng.s0), s1: acc.s1.bitwise_xor(rng.s1), s2: acc.s2.bitwise_xor(rng.s2), s3: acc.s3.bitwise_xor(rng.s3) }
				} else {
					acc
				}
			(_, stepped) = next_u64(rng)
			jump_word({ acc: next_acc, rng: stepped }, constant, bit + 1)
		}

	rotl : U64, U8 -> U64
	rotl = |x, n| x.shl_wrap(n).bitwise_or(x.shr_zf_wrap(64 - n))
}

take_words : FastRng, U64 -> List(U64)
take_words = |rng, count| take_words_help(rng, count, List.with_capacity(count))

take_words_help : FastRng, U64, List(U64) -> List(U64)
take_words_help = |rng, count, acc|
	if count == 0 {
		acc
	} else {
		(word, next) = rng.next_u64()
		take_words_help(next, count - 1, acc.append(word))
	}

expect
	RngVectors.xoshiro_streams.all(|v|
		match FastRng.from_words(v.seed) {
			Ok(rng) => take_words(rng, v.words.len()) == v.words and take_words(FastRng.jump(rng), v.jumped.len()) == v.jumped
			Err(_) => False
		})

expect
	RngVectors.xoshiro_from_u64.all(|v| take_words(FastRng.from_u64(v.seed), v.words.len()) == v.words)

expect
	RngVectors.xoshiro_streams.all(|v|
		match FastRng.from_words(v.seed) {
			Ok(rng) => {
				(child, parent) = rng.fork()
				(second_child, _) = parent.fork()
				take_words(child, v.words.len()) == v.words
				and take_words(parent, v.jumped.len()) == v.jumped
				and take_words(second_child, v.jumped.len()) == v.jumped
				and take_words(second_child, 1) != take_words(child, 1)
			}
			Err(_) => False
		})

expect
	match FastRng.from_words({ w0: 0, w1: 0, w2: 0, w3: 0 }) {
		Err(AllZero) => True
		_ => False
	}

# Saved state is the four words, little-endian, and resumes the sequence.
expect
	RngVectors.xoshiro_streams.all(|v|
		match FastRng.from_words(v.seed) {
			Ok(rng) => {
				(_, advanced) = rng.next_u64()
				saved = advanced.to_bytes()
				first = U64.from_le_bytes(saved, 0) ?? 0
				expected_first = FastRng.from_words(v.seed).map_ok(|r| r.next_u64().1.s0) ?? 1
				match FastRng.from_bytes(saved) {
					Ok(resumed) => saved.len() == 32 and first == expected_first and take_words(resumed, 5) == take_words(advanced, 5)
					Err(_) => False
				}
			}
			Err(_) => False
		})

expect
	[List.repeat(0, 32), List.repeat(1, 31), List.repeat(1, 33), []].all(|bytes|
		match FastRng.from_bytes(bytes) {
			Err(InvalidState) => True
			_ => False
		})
