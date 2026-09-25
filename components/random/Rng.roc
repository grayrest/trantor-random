import IOErr exposing [IOErr]
import Random
import ChaCha8
import SplitMix64
import RngDraw
import Weights
import RngVectors

## The default random number generator: C2SP chacha8rand.
##
## Its output cannot be predicted from earlier output, so it is the one to use
## for anything someone might want to guess: UUIDs, tokens, shuffles with
## something riding on them. It is not a cryptography API.
##
## A generator is a value. Every draw returns the value drawn and the
## generator to draw from next:
##
## ```roc
## rng = Rng.from_os!()?
## (roll, rng2) = rng.between_u64(1, 6)
## ```
##
## For a given seed the sequence is the same in every 1.x release.
Rng :: { seed : Seed, buf : List(U64), i : U64, n : U64, c : U32 }.{

	## Four words of seed.
	Seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }

	## A generator seeded with 256 bits from the operating system.
	from_os! : () => Try(Rng, [RandomErr(IOErr)])
	from_os! = || {
		w0 = Random.seed_u64!()?
		w1 = Random.seed_u64!()?
		w2 = Random.seed_u64!()?
		w3 = Random.seed_u64!()?
		Ok(from_words({ w0, w1, w2, w3 }))
	}

	## A generator from an exact 256-bit seed. The 32 seed bytes are these
	## words, each little-endian.
	from_words : Seed -> Rng
	from_words = |seed| Rng.{ seed, buf: ChaCha8.fill(List.repeat(0, words_per_fill), seed, 0), i: 0, n: words_per_fill, c: 0 }

	## A generator from a `U64`, for tests and reproducible runs. The seed is
	## expanded to 256 bits with SplitMix64; it carries 64 bits of entropy at
	## most.
	from_u64 : U64 -> Rng
	from_u64 = |seed| from_words(SplitMix64.expand(seed))

	## The next 64 random bits.
	next_u64 : Rng -> (U64, Rng)
	next_u64 = |rng| {
		ready = if rng.i < rng.n { rng } else { refill(rng) }
		(ready.buf.get(ready.i) ?? 0, { ..ready, i: ready.i + 1 })
	}

	# Draws: each forwards to `RngDraw`, where they are documented in full.

	## 64 random bits.
	u64 : Rng -> (U64, Rng)
	u64 = |rng| RngDraw.u64(rng)

	## 32 random bits.
	u32 : Rng -> (U32, Rng)
	u32 = |rng| RngDraw.u32(rng)

	## 8 random bits.
	u8 : Rng -> (U8, Rng)
	u8 = |rng| RngDraw.u8(rng)

	## `True` or `False`.
	bool : Rng -> (Bool, Rng)
	bool = |rng| RngDraw.bool(rng)

	## A number in `[0, n)`; `n` of 0 crashes.
	below : Rng, U64 -> (U64, Rng)
	below = |rng, n| RngDraw.below(rng, n)

	## A number in `[lo, hi]`; reversed bounds are swapped.
	between_u64 : Rng, U64, U64 -> (U64, Rng)
	between_u64 = |rng, lo, hi| RngDraw.between_u64(rng, lo, hi)

	## A number in `[lo, hi]`; reversed bounds are swapped.
	between_i64 : Rng, I64, I64 -> (I64, Rng)
	between_i64 = |rng, lo, hi| RngDraw.between_i64(rng, lo, hi)

	## A float in `[0, 1)`.
	f64 : Rng -> (F64, Rng)
	f64 = |rng| RngDraw.f64(rng)

	## A float in `[lo, hi)`; see `RngDraw.between_f64`.
	between_f64 : Rng, F64, F64 -> (F64, Rng)
	between_f64 = |rng, lo, hi| RngDraw.between_f64(rng, lo, hi)

	## `True` with probability `p`.
	chance : Rng, F64 -> (Bool, Rng)
	chance = |rng, p| RngDraw.chance(rng, p)

	## `n` random bytes.
	bytes : Rng, U64 -> (List(U8), Rng)
	bytes = |rng, n| RngDraw.bytes(rng, n)

	## `list` in random order.
	shuffle : Rng, List(a) -> (List(a), Rng)
	shuffle = |rng, list| RngDraw.shuffle(rng, list)

	## One item of `list`, each equally likely.
	choose : Rng, List(a) -> (Try(a, [ListWasEmpty]), Rng)
	choose = |rng, list| RngDraw.choose(rng, list)

	## `k` different items of `list`, in the order drawn.
	sample : Rng, List(a), U64 -> (List(a), Rng)
	sample = |rng, list, k| RngDraw.sample(rng, list, k)

	## An item of `weights`, each in proportion to its weight.
	pick : Rng, Weights(a) -> (a, Rng)
	pick = |rng, weights| RngDraw.pick(rng, weights)

	## `(child, parent)`: a new generator seeded from four of this one's draws,
	## and this generator after them. The two sequences are independent.
	fork : Rng -> (Rng, Rng)
	fork = |rng| {
		(w0, r1) = next_u64(rng)
		(w1, r2) = next_u64(r1)
		(w2, r3) = next_u64(r2)
		(w3, parent) = next_u64(r3)
		(from_words({ w0, w1, w2, w3 }), parent)
	}

	## This generator's position, as bytes that `from_bytes` turns back into a
	## generator continuing the same sequence. Bytes saved by any 1.x release
	## load in any later 1.x release.
	to_bytes : Rng -> List(U8)
	to_bytes = |rng| {
		used = rng.c.to_u64() // 4 * words_per_fill + rng.i
		state_tag
			.concat(be_bytes(used))
			.concat(le_bytes(rng.seed.w0))
			.concat(le_bytes(rng.seed.w1))
			.concat(le_bytes(rng.seed.w2))
			.concat(le_bytes(rng.seed.w3))
	}

	## The generator `to_bytes` saved.
	from_bytes : List(U8) -> Try(Rng, [InvalidState])
	from_bytes = |data| {
		# 48 bytes: the tag, the words used since the last reseed (big-endian),
		# then the seed (little-endian). A count past 124 is never written.
		well_formed = data.len() == state_len and data.take_first(state_tag.len()) == state_tag
		used = be_u64(data, 8)
		if !well_formed or used > words_per_key - 4 {
			Err(InvalidState)
		} else {
			seed = {
				w0: U64.from_le_bytes(data, 16) ?? 0,
				w1: U64.from_le_bytes(data, 24) ?? 0,
				w2: U64.from_le_bytes(data, 32) ?? 0,
				w3: U64.from_le_bytes(data, 40) ?? 0,
			}
			counter = (used // words_per_fill * 4).to_u32_wrap()
			Ok(Rng.{
				seed,
				buf: ChaCha8.fill(List.repeat(0, words_per_fill), seed, counter),
				i: used % words_per_fill,
				n: if counter == blocks_per_key - 4 { words_per_fill - 4 } else { words_per_fill },
				c: counter,
			})
		}
	}

	## Go's `chacha8rand.State.Refill`. Every fourth fill (16 blocks) the last
	## four words of the previous buffer become the seed and are never output,
	## which is why that buffer holds 28 usable words.
	refill : Rng -> Rng
	refill = |rng| {
		next_c = rng.c + 4
		reseeding = next_c == blocks_per_key
		new_seed =
			if reseeding {
				{ w0: rng.buf.get(28) ?? 0, w1: rng.buf.get(29) ?? 0, w2: rng.buf.get(30) ?? 0, w3: rng.buf.get(31) ?? 0 }
			} else {
				rng.seed
			}
		counter = if reseeding { 0 } else { next_c }
		Rng.{
			seed: new_seed,
			buf: ChaCha8.fill(rng.buf, new_seed, counter),
			i: 0,
			n: if counter == blocks_per_key - 4 { words_per_fill - 4 } else { words_per_fill },
			c: counter,
		}
	}

	words_per_fill : U64
	words_per_fill = 32

	## Words produced under one seed: four fills.
	words_per_key : U64
	words_per_key = 128

	state_tag : List(U8)
	state_tag = Str.to_utf8("chacha8:")

	state_len : U64
	state_len = 48

	le_bytes : U64 -> List(U8)
	le_bytes = |word| [0, 8, 16, 24, 32, 40, 48, 56].map(|shift| word.shr_zf_wrap(shift).to_u8_wrap())

	be_bytes : U64 -> List(U8)
	be_bytes = |word| [56, 48, 40, 32, 24, 16, 8, 0].map(|shift| word.shr_zf_wrap(shift).to_u8_wrap())

	be_u64 : List(U8), U64 -> U64
	be_u64 = |data, at| data.sublist({ start: at, len: 8 }).fold(0, |acc, byte| acc.shl_wrap(8).bitwise_or(byte.to_u64()))

	blocks_per_key : U32
	blocks_per_key = 16
}

## `count` words from `rng`, in order.
take_words : Rng, U64 -> List(U64)
take_words = |rng, count| take_words_help(rng, count, List.with_capacity(count))

take_words_help : Rng, U64, List(U64) -> List(U64)
take_words_help = |rng, count, acc|
	if count == 0 {
		acc
	} else {
		(word, next) = rng.next_u64()
		take_words_help(next, count - 1, acc.append(word))
	}

expect
	RngVectors.chacha_streams.all(|stream| take_words(Rng.from_words(stream.seed), stream.words.len()) == stream.words)

expect
	RngVectors.chacha_forks.all(|f| {
		(_, skipped) = skip_words(Rng.from_words(f.seed), f.skip)
		(child, parent) = skipped.fork()
		take_words(child, f.child.len()) == f.child and take_words(parent, f.parent.len()) == f.parent
	})

skip_words : Rng, U64 -> ({}, Rng)
skip_words = |rng, count|
	if count == 0 {
		({}, rng)
	} else {
		(_, next) = rng.next_u64()
		skip_words(next, count - 1)
	}

expect
	RngVectors.chacha_states.all(|s| {
		(_, drawn) = skip_words(Rng.from_words(s.seed), s.draws)
		saved = drawn.to_bytes()
		resumed_matches =
			match Rng.from_bytes(saved) {
				Ok(resumed) => take_words(resumed, s.next.len()) == s.next
				Err(_) => False
			}
		saved == s.state and resumed_matches and take_words(drawn, s.next.len()) == s.next
	})

expect
	RngVectors.chacha_unmarshal.all(|u| Rng.from_bytes(u.state).is_ok() == u.ok)

# Refused: other lengths, another tag, and a longer form with a prefix.
expect {
	good = Rng.from_u64(7).to_bytes()
	short = good.drop_last(1)
	retagged = good.set(0, 'C') ?? []
	readbuf = Str.to_utf8("readbuf:").concat(List.repeat(1, 6)).concat(good)
	[short, retagged, readbuf, good.append(0), []].all(|bytes| !Rng.from_bytes(bytes).is_ok())
}
