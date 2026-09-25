## Items with integer weights, for `pick`: each item comes up in proportion
## to its weight. Build once, pick many times.
##
## ```roc
## loot = Weights.from_list([(Common, 70), (Rare, 25), (Legendary, 5)])?
## (drop, rng2) = rng.pick(loot)
## ```
##
## Weights are integers so the choice is exact: no rounding decides which item
## comes up, and a given seed picks the same items in every 1.x release.
Weights(a) :: { items : List(a), cumulative : List(U64), total : U64 }.{

	## The table for `entries`. An entry with weight 0 is never picked.
	from_list : List((a, U64)) -> Try(Weights(a), [ListWasEmpty, AllZero, TotalOverflow])
	from_list = |entries|
		if entries.is_empty() {
			Err(ListWasEmpty)
		} else {
			match running_totals(entries.map(|(_, weight)| weight)) {
				Err(TotalOverflow) => Err(TotalOverflow)
				Ok(sums) =>
					if sums.total == 0 {
						Err(AllZero)
					} else {
						Ok(Weights.{ items: entries.map(|(item, _)| item), cumulative: sums.cumulative, total: sums.total })
					}
			}
		}

	## The sum of the weights.
	total : Weights(a) -> U64
	total = |weights| weights.total

	## The item whose share of `[0, total)` holds `x`: the first entry whose
	## running total is greater than `x`. `x` is taken modulo the total.
	index_for : Weights(a), U64 -> a
	index_for = |weights, x| {
		target = x % weights.total
		index = first_above(weights.cumulative, target, 0, weights.cumulative.len())
		match weights.items.get(index) {
			Ok(item) => item
			Err(_) => crash "Weights.index_for: no entry above ${target.to_str()}, which from_list rules out"
		}
	}

	running_totals : List(U64) -> Try({ cumulative : List(U64), total : U64 }, [TotalOverflow])
	running_totals = |weights|
		weights.fold_try({ cumulative: List.with_capacity(weights.len()), total: 0 }, |acc, weight|
			match acc.total.plus_try(weight) {
				Ok(sum) => Ok({ cumulative: acc.cumulative.append(sum), total: sum })
				Err(_) => Err(TotalOverflow)
			})

	## Binary search in `[low, high)` for the first running total above `x`.
	first_above : List(U64), U64, U64, U64 -> U64
	first_above = |cumulative, x, low, high|
		if low >= high {
			low
		} else {
			mid = low + (high - low) // 2
			if (cumulative.get(mid) ?? 0) > x {
				first_above(cumulative, x, low, mid)
			} else {
				first_above(cumulative, x, mid + 1, high)
			}
		}
}

expect
	match Weights.from_list([]) {
		Err(ListWasEmpty) => True
		_ => False
	}

expect
	match Weights.from_list([(A, 0), (B, 0)]) {
		Err(AllZero) => True
		_ => False
	}

expect
	match Weights.from_list([(A, U64.highest), (B, 1)]) {
		Err(TotalOverflow) => True
		_ => False
	}

expect
	match Weights.from_list([("a", 0), ("b", 2), ("c", 0), ("d", 3)]) {
		Ok(w) => w.total() == 5 and [0, 1, 2, 3, 4, 5].map(|x| w.index_for(x)) == ["b", "b", "d", "d", "d", "b"]
		Err(_) => False
	}
