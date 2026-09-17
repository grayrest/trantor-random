import Rng
import FastRng
import RngDraw
import Weights
import RngVectors

## Checks every draw against `RngVectors`, which a Rust model computed from
## the documented algorithms: through `Rng`'s methods, `FastRng`'s methods,
## and `RngDraw` on each. After each case the next word must match too, so a
## draw that uses one word too many or too few fails. Generated with the
## vectors' shapes in mind; it holds no data of its own.
RngDrawCheck :: [].{

	chacha : () -> Rng
	chacha = || Rng.from_words((RngVectors.chacha_streams.get(1) ?? crash "chacha stream 1").seed)

	fast : () -> FastRng
	fast = || FastRng.from_words((RngVectors.xoshiro_streams.get(1) ?? crash "xoshiro stream 1").seed) ?? crash "xoshiro seed 1 is not zero"

	## The list every list draw works on: 1, 11, 21, ...
	items : U64 -> List(U64)
	items = |len| List.repeat(0, len).map_with_index(|_, i| i * 10 + 1)

	weights_of : List(U64) -> Weights(U64)
	weights_of = |weights|
		match Weights.from_list(weights.map_with_index(|weight, i| (i, weight))) {
			Ok(table) => table
			Err(_) => crash "the vectors only hold valid weights"
		}

	repeat : r, U64, (r -> (a, r)) -> (List(a), r)
	repeat = |g, count, draw| repeat_help(g, count, draw, List.with_capacity(count))

	repeat_help : r, U64, (r -> (a, r)), List(a) -> (List(a), r)
	repeat_help = |g, count, draw, acc|
		if count == 0 {
			(acc, g)
		} else {
			(value, next) = draw(g)
			repeat_help(next, count - 1, draw, acc.append(value))
		}
}

expect
	RngVectors.draw_u64.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.u64())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.u64(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.u64())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.u64(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_u32.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.u32())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.u32(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.u32())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.u32(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_u8.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.u8())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.u8(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.u8())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.u8(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_bool.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.bool())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.bool(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.bool())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.bool(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_below.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.below(v.n))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.below(g, v.n))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.below(v.n))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.below(g, v.n))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_between_u64.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.between_u64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.between_u64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.between_u64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.between_u64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_between_i64.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.between_i64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.between_i64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.between_i64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.between_i64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_f64.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.f64())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.f64(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.f64())
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.f64(g))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_between_f64.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.between_f64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.between_f64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.between_f64(v.lo, v.hi))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.between_f64(g, v.lo, v.hi))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_chance.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.chance(v.p))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.chance(g, v.p))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.chance(v.p))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.chance(g, v.p))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_bytes.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = {
					g = RngDrawCheck.chacha()
					g.bytes(v.n)
				}
				(d, dr) = {
					g = RngDrawCheck.chacha()
					RngDraw.bytes(g, v.n)
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = {
					g = RngDrawCheck.fast()
					g.bytes(v.n)
				}
				(d, dr) = {
					g = RngDrawCheck.fast()
					RngDraw.bytes(g, v.n)
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_shuffle.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = {
					g = RngDrawCheck.chacha()
					g.shuffle(RngDrawCheck.items(v.len))
				}
				(d, dr) = {
					g = RngDrawCheck.chacha()
					RngDraw.shuffle(g, RngDrawCheck.items(v.len))
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = {
					g = RngDrawCheck.fast()
					g.shuffle(RngDrawCheck.items(v.len))
				}
				(d, dr) = {
					g = RngDrawCheck.fast()
					RngDraw.shuffle(g, RngDrawCheck.items(v.len))
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_choose.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = {
					g = RngDrawCheck.chacha()
					g.choose(RngDrawCheck.items(v.len))
				}
				(d, dr) = {
					g = RngDrawCheck.chacha()
					RngDraw.choose(g, RngDrawCheck.items(v.len))
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = {
					g = RngDrawCheck.fast()
					g.choose(RngDrawCheck.items(v.len))
				}
				(d, dr) = {
					g = RngDrawCheck.fast()
					RngDraw.choose(g, RngDrawCheck.items(v.len))
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_sample.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = {
					g = RngDrawCheck.chacha()
					g.sample(RngDrawCheck.items(v.len), v.k)
				}
				(d, dr) = {
					g = RngDrawCheck.chacha()
					RngDraw.sample(g, RngDrawCheck.items(v.len), v.k)
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = {
					g = RngDrawCheck.fast()
					g.sample(RngDrawCheck.items(v.len), v.k)
				}
				(d, dr) = {
					g = RngDrawCheck.fast()
					RngDraw.sample(g, RngDrawCheck.items(v.len), v.k)
				}
				m == v.result and d == v.result and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})

expect
	RngVectors.draw_pick.all(|v|
		match v.source {
			Chacha => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| g.pick(RngDrawCheck.weights_of(v.weights)))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.chacha(), 6, |g| RngDraw.pick(g, RngDrawCheck.weights_of(v.weights)))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
			Fast => {
				(m, mr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| g.pick(RngDrawCheck.weights_of(v.weights)))
				(d, dr) = RngDrawCheck.repeat(RngDrawCheck.fast(), 6, |g| RngDraw.pick(g, RngDrawCheck.weights_of(v.weights)))
				m == v.results and d == v.results and mr.next_u64().0 == v.next and dr.next_u64().0 == v.next
			}
		})
