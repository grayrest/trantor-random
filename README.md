# trantor-random

Seeded random number generators, the draws built on them, and UUIDs, for
[trantor-cli](../trantor-cli) apps. Everything after seeding is pure Roc.

```toml
[deps]
trantor-cli    = { path = "../trantor-cli" }
trantor-random = { path = "../trantor-random" }
```

## Two generators

- **`Rng`** is ChaCha8 (C2SP chacha8rand). Its output cannot be predicted
  from earlier output, so use it for anything someone might want to guess:
  UUIDs, tokens, a shuffle with something riding on it. It is not a
  cryptography API.
- **`FastRng`** is xoshiro256++, about seven times faster per draw. Anyone who
  sees four of its outputs can compute the rest. Use it for simulations, games
  and tests.

Seed either one from the operating system with `from_os!()`, from a `U64`
with `from_u64` (reproducible runs and tests), or from four exact words with
`from_words`.

## Drawing

A generator is a value. Every draw returns what it drew and the generator to
draw from next:

```roc
roll_two : Rng -> ((U64, U64), Rng)
roll_two = |rng| {
	(first, rng2) = rng.between_u64(1, 6)
	(second, rng3) = rng2.between_u64(1, 6)
	((first, second), rng3)
}
```

In a loop, keep the generator in a `var`:

```roc
roll_many : Rng, U64 -> (List(U64), Rng)
roll_many = |start, count| {
	var $rng = start
	var $rolls = List.with_capacity(count)
	for _ in 0.U64.until(count) {
		(roll, $rng) = $rng.between_u64(1, 6)
		$rolls = $rolls.append(roll)
	}
	($rolls, $rng)
}
```

| Draw | Gives |
|---|---|
| `u64`, `u32`, `u8`, `bool` | random bits |
| `below(n)` | `[0, n)`, uniform; `n` of 0 crashes |
| `between_u64(lo, hi)`, `between_i64(lo, hi)` | `[lo, hi]`, uniform, bounds in either order |
| `f64` | `[0, 1)` |
| `between_f64(lo, hi)` | `[lo, hi)`, bounds in either order |
| `chance(p)` | `True` with probability `p` |
| `bytes(n)` | `n` random bytes |
| `shuffle(list)` | `list` in random order |
| `choose(list)` | one item, or `Err(ListWasEmpty)` |
| `sample(list, k)` | `k` different items, in the order drawn |
| `pick(weights)` | an item in proportion to its weight |

Other integer types come from `between_u64` or `between_i64` and a builtin
conversion. `Rng` and `FastRng` have every draw as a method; `RngDraw` has
them as functions that take any value with
`next_u64 : r -> (U64, r)`, so a generator of your own gets all of them.

### Weights

```roc
Loot : [Common, Rare, Legendary]

loot_table : () -> Try(Weights(Loot), [ListWasEmpty, AllZero, TotalOverflow])
loot_table = || Weights.from_list([(Common, 70), (Rare, 25), (Legendary, 5)])
```

Build the table once and `pick` from it as often as you like. Weights are
integers, so no rounding decides what comes up. An entry with weight 0 never
does.

### Fork and save

`rng.fork()` returns `(child, parent)`: two generators whose sequences do not
overlap, for handing one to a worker. `rng.to_bytes()` saves a generator's
position and `Rng.from_bytes` resumes it (`FastRng` the same).

## UUIDs

```roc
(id, rng2) = Uuid.v4(rng)
Stdout.line!(id.to_str())?
```

- **`Uuid.v4(rng)`** is 122 random bits.
- **`Uuid.v7(rng, unix_ms)`** puts the time first, so UUIDs sort by when they
  were made. Several in one millisecond come out in random order.
- **`UuidV7`** is a stream that keeps them in order within a millisecond and
  when the clock steps back. Take the time from trantor-cli:

  ```roc
  now = Utc.to_millis_since_epoch(Utc.now!()).to_u64_wrap()
  ids = UuidV7.new(rng)
  (id, ids2) = ids.next(now)
  ```

A `Uuid` compares in RFC byte order, works as a `Dict` key, and encodes as
its string form through any format. `from_str` accepts only the
hyphenated `8-4-4-4-12` form, in either case.

On the current compiler a `Uuid` decodes through `Json.parse` on its own but
not as a field of a record; this is a compiler gap that affects any nominal
type with a format-generic parser.

## Stability

For a given seed, both generators, every draw and both UUID versions produce
the same values in every 1.x release, and saved state loads in any later 1.x
release. Changing any of that is a major version.
