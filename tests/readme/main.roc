app [main!] { pf: platform "../target/trantor/app/platform/main.roc" }

import pf.OsStr exposing [OsStr]
import pf.Stdout
import pf.Utc
import pf.Rng
import pf.FastRng
import pf.RngDraw
import pf.Weights
import pf.Uuid
import pf.UuidV7

## README.md's examples, verbatim, so the README keeps compiling. The output
## prints only what does not depend on the OS seed.

## Two dice from a generator, and the generator to use next.
roll_two : Rng -> ((U64, U64), Rng)
roll_two = |rng| {
	(first, rng2) = rng.between_u64(1, 6)
	(second, rng3) = rng2.between_u64(1, 6)
	((first, second), rng3)
}

## The same, in a loop over a `var`.
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

Loot : [Common, Rare, Legendary]

loot_table : () -> Try(Weights(Loot), [ListWasEmpty, AllZero, TotalOverflow, ..])
loot_table = || Weights.from_list([(Common, 70), (Rare, 25), (Legendary, 5)])

## A generator of your own: a counter, which every RngDraw function accepts.
Counter :: { n : U64 }.{
	next_u64 : Counter -> (U64, Counter)
	next_u64 = |c| (c.n, { n: c.n + 1 })
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	rng = Rng.from_os!()?
	((a, b), rng2) = roll_two(rng)
	Stdout.line!("two dice in range: ${Str.inspect(a >= 1 and a <= 6 and b >= 1 and b <= 6)}")?

	(rolls, rng3) = roll_many(rng2, 100)
	Stdout.line!("a hundred rolls: ${Str.inspect(rolls.len() == 100 and rolls.all(|r| r >= 1 and r <= 6))}")?

	# Reproducible: the same seed gives the same draws.
	(x, _) = FastRng.from_u64(42).below(1000)
	(y, _) = FastRng.from_u64(42).below(1000)
	Stdout.line!("same seed, same draw: ${Str.inspect(x == y)}")?

	# Hand a stream to a worker without sharing one.
	(child, parent) = rng3.fork()
	(c, _) = child.u64()
	(p, rng4) = parent.u64()
	Stdout.line!("fork gives two streams: ${Str.inspect(c != p)}")?

	# Save and resume.
	saved = rng4.to_bytes()
	resumed = Rng.from_bytes(saved)?
	Stdout.line!("resumed where it left off: ${Str.inspect(resumed.u64().0 == rng4.u64().0)}")?

	(deck, rng5) = rng4.shuffle(["A", "K", "Q", "J"])
	(hand, rng6) = rng5.sample(deck, 2)
	Stdout.line!("shuffle and sample: ${Str.inspect(deck.len() == 4 and hand.len() == 2)}")?

	table = loot_table()?
	(drop, rng7) = rng6.pick(table)
	Stdout.line!("a drop: ${Str.inspect(drop == Common or drop == Rare or drop == Legendary)}")?

	(id, rng8) = Uuid.v4(rng7)
	Stdout.line!("v4 version: ${Str.inspect(id.version())}, parses back: ${Str.inspect(Uuid.from_str(id.to_str()) == Ok(id))}")?

	now = Utc.to_millis_since_epoch(Utc.now!()).to_u64_wrap()
	ids = UuidV7.new(rng8)
	(first, ids2) = ids.next(now)
	(second, _) = ids2.next(now)
	Stdout.line!("v7 in order: ${Str.inspect(first < second)}")?

	(counted, _) = RngDraw.below(Counter.{ n: 0 }, 10)
	Stdout.line!("a custom generator draws: ${Str.inspect(counted)}")
}
