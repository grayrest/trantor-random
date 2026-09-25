import Weights

## Every draw, for any generator: a value with
## `next_u64 : r -> (U64, r)`.
##
## `Rng` and `FastRng` have each of these as a method, so `rng.below(6)` is
## `RngDraw.below(rng, 6)`. Call these directly for a generator of your own.
##
## Each draw returns the value and the generator to draw from next. The
## algorithms are fixed: for a given generator state every draw returns the
## same value, and uses the same number of words, in every 1.x release.
RngDraw :: [].{

	## 64 random bits.
	u64 : r -> (U64, r) where [r.next_u64 : r -> (U64, r)]
	u64 = |rng| rng.next_u64()

	## 32 random bits: the high half of one word.
	u32 : r -> (U32, r) where [r.next_u64 : r -> (U64, r)]
	u32 = |rng| {
		(word, next) = rng.next_u64()
		(word.shr_zf_wrap(32).to_u32_wrap(), next)
	}

	## 8 random bits: the top byte of one word.
	u8 : r -> (U8, r) where [r.next_u64 : r -> (U64, r)]
	u8 = |rng| {
		(word, next) = rng.next_u64()
		(word.shr_zf_wrap(56).to_u8_wrap(), next)
	}

	## `True` or `False`, from the top bit of one word.
	bool : r -> (Bool, r) where [r.next_u64 : r -> (U64, r)]
	bool = |rng| {
		(word, next) = rng.next_u64()
		(word.shr_zf_wrap(63) == 1, next)
	}

	## A number in `[0, n)`, every value equally likely (Lemire's method: one
	## word almost always, more when a draw would bias the result). `n` of 0
	## crashes.
	below : r, U64 -> (U64, r) where [r.next_u64 : r -> (U64, r)]
	below = |rng, n| {
		if n == 0 {
			crash "RngDraw.below: n is 0, and [0, 0) is empty"
		}
		(word, next) = rng.next_u64()
		product = word.to_u128() * n.to_u128()
		if product.to_u64_wrap() < n {
			threshold = (0.U64).minus_wrap(n) % n
			below_retry(next, n, threshold, product)
		} else {
			(product.shr_zf_wrap(64).to_u64_wrap(), next)
		}
	}

	below_retry : r, U64, U64, U128 -> (U64, r) where [r.next_u64 : r -> (U64, r)]
	below_retry = |rng, n, threshold, product|
		if product.to_u64_wrap() < threshold {
			(word, next) = rng.next_u64()
			below_retry(next, n, threshold, word.to_u128() * n.to_u128())
		} else {
			(product.shr_zf_wrap(64).to_u64_wrap(), rng)
		}

	## A number in `[lo, hi]`, every value equally likely. Reversed bounds are
	## swapped.
	between_u64 : r, U64, U64 -> (U64, r) where [r.next_u64 : r -> (U64, r)]
	between_u64 = |rng, a, b| {
		(lo, hi) = if a > b { (b, a) } else { (a, b) }
		span = hi - lo
		if span == U64.highest {
			rng.next_u64()
		} else {
			(offset, next) = below(rng, span + 1)
			(lo + offset, next)
		}
	}

	## A number in `[lo, hi]`, every value equally likely. Reversed bounds are
	## swapped.
	between_i64 : r, I64, I64 -> (I64, r) where [r.next_u64 : r -> (U64, r)]
	between_i64 = |rng, a, b| {
		(value, next) = between_u64(rng, flip_sign(a.to_u64_wrap()), flip_sign(b.to_u64_wrap()))
		(flip_sign(value).to_i64_wrap(), next)
	}

	## A float in `[0, 1)`: the top 53 bits of one word, scaled.
	f64 : r -> (F64, r) where [r.next_u64 : r -> (U64, r)]
	f64 = |rng| {
		(word, next) = rng.next_u64()
		(word.shr_zf_wrap(11).to_f64() * unit, next)
	}

	## A float in `[lo, hi)`. Reversed bounds are swapped. Equal bounds return
	## `lo` without drawing, infinite ones included. A result that rounds up to
	## `hi` is drawn again. Otherwise, bounds so far apart that `hi - lo` is
	## not finite, an infinite bound, or a NaN crash.
	between_f64 : r, F64, F64 -> (F64, r) where [r.next_u64 : r -> (U64, r)]
	between_f64 = |rng, a, b| {
		(lo, hi) = if a > b { (b, a) } else { (a, b) }
		if lo == hi {
			(lo, rng)
		} else {
			span = hi - lo
			if !span.is_finite() {
				crash "RngDraw.between_f64: the span from ${lo.to_str()} to ${hi.to_str()} is not a finite number"
			}
			between_f64_retry(rng, lo, hi, span)
		}
	}

	between_f64_retry : r, F64, F64, F64 -> (F64, r) where [r.next_u64 : r -> (U64, r)]
	between_f64_retry = |rng, lo, hi, span| {
		(fraction, next) = f64(rng)
		x = lo + fraction * span
		if x < hi {
			(x, next)
		} else {
			between_f64_retry(next, lo, hi, span)
		}
	}

	## `True` with probability `p`: `f64() < p`. `p` of 0 or less is never,
	## 1 or more always; every call uses one word.
	chance : r, F64 -> (Bool, r) where [r.next_u64 : r -> (U64, r)]
	chance = |rng, p| {
		(fraction, next) = f64(rng)
		(fraction < p, next)
	}

	## `n` random bytes: whole words, little-endian, the last one cut short.
	bytes : r, U64 -> (List(U8), r) where [r.next_u64 : r -> (U64, r)]
	bytes = |rng, n| bytes_help(rng, n, List.with_capacity(n))

	bytes_help : r, U64, List(U8) -> (List(U8), r) where [r.next_u64 : r -> (U64, r)]
	bytes_help = |rng, n, acc| {
		have = acc.len()
		if have >= n {
			(acc, rng)
		} else {
			(word, next) = rng.next_u64()
			take = if n - have < 8 { n - have } else { 8 }
			bytes_help(next, n, append_le(acc, word, take))
		}
	}

	## `list` in random order, every order equally likely (Fisher–Yates,
	## from the end). Lists of 0 or 1 items draw nothing.
	shuffle : r, List(a) -> (List(a), r) where [r.next_u64 : r -> (U64, r)]
	shuffle = |rng, list| {
		len = list.len()
		if len < 2 {
			(list, rng)
		} else {
			shuffle_help(rng, list, len - 1)
		}
	}

	shuffle_help : r, List(a), U64 -> (List(a), r) where [r.next_u64 : r -> (U64, r)]
	shuffle_help = |rng, list, i|
		if i == 0 {
			(list, rng)
		} else {
			(j, next) = below(rng, i + 1)
			shuffle_help(next, swap_at(list, i, j), i - 1)
		}

	## One item of `list`, each equally likely. An empty list draws nothing.
	choose : r, List(a) -> (Try(a, [ListWasEmpty]), r) where [r.next_u64 : r -> (U64, r)]
	choose = |rng, list|
		if list.is_empty() {
			(Err(ListWasEmpty), rng)
		} else {
			(index, next) = below(rng, list.len())
			match list.get(index) {
				Ok(item) => (Ok(item), next)
				Err(_) => crash "RngDraw.choose: index ${index.to_str()} is below the length, so it is in range"
			}
		}

	## `k` different items of `list`, in the order drawn. Asking for more items
	## than the list has gives all of them, in random order.
	##
	## The walk is Fisher–Yates from the front, stopped after `k` steps; every
	## step draws, including the last one of a full walk. So `sample` of the
	## whole list is a shuffle, but not the same one `shuffle` gives.
	sample : r, List(a), U64 -> (List(a), r) where [r.next_u64 : r -> (U64, r)]
	sample = |rng, list, k| {
		len = list.len()
		take = if k < len { k } else { len }
		(walked, next) = sample_help(rng, list, 0, take)
		(walked.take_first(take), next)
	}

	sample_help : r, List(a), U64, U64 -> (List(a), r) where [r.next_u64 : r -> (U64, r)]
	sample_help = |rng, list, i, take|
		if i >= take {
			(list, rng)
		} else {
			(offset, next) = below(rng, list.len() - i)
			sample_help(next, swap_at(list, i, i + offset), i + 1, take)
		}

	## An item of `weights`, each in proportion to its weight.
	pick : r, Weights(a) -> (a, r) where [r.next_u64 : r -> (U64, r)]
	pick = |rng, weights| {
		(x, next) = below(rng, weights.total())
		(weights.index_for(x), next)
	}

	## 2^-53.
	unit : F64
	unit = 1.1102230246251565e-16

	flip_sign : U64 -> U64
	flip_sign = |x| x.bitwise_xor(0x8000000000000000)

	append_le : List(U8), U64, U64 -> List(U8)
	append_le = |acc, word, count|
		if count == 0 {
			acc
		} else {
			append_le(acc.append(word.to_u8_wrap()), word.shr_zf_wrap(8), count - 1)
		}

	## Both indexes are always in range here. The error branch does not mention
	## `list`, so a unique list is swapped in place.
	swap_at : List(a), U64, U64 -> List(a)
	swap_at = |list, i, j|
		match list.swap(i, j) {
			Ok(swapped) => swapped
			Err(_) => crash "RngDraw.swap_at: ${i.to_str()} or ${j.to_str()} out of range"
		}
}

## A generator that returns its words in order, then zeros.
Script :: { words : List(U64), at : U64 }.{
	next_u64 : Script -> (U64, Script)
	next_u64 = |script| (script.words.get(script.at) ?? 0, { ..script, at: script.at + 1 })
}

script : List(U64) -> Script
script = |words| Script.{ words, at: 0 }

# The largest fraction below 1 lands on `hi` for [1, 3) and must be redrawn.
expect {
	(x, rest) = RngDraw.between_f64(script([U64.highest, 0]), 1.0, 3.0)
	x == 1.0 and rest.at == 2
}

expect {
	(x, rest) = RngDraw.between_f64(script([7]), 2.5, 2.5)
	x == 2.5 and rest.at == 0
}

# 2^-53 is exact.
expect RngDraw.unit == F64.from_bits(0x3ca0000000000000)

# The words below `threshold` are rejected, and the draw moves on.
expect {
	n = 0x8000000000000001
	(value, rest) = RngDraw.below(script([0, 2, U64.highest]), n)
	rest.at == 3 and value == 0x8000000000000000
}
