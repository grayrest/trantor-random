import Uuid
import Rng
import FastRng
import RngVectors

## A stream of version 7 UUIDs that sort in the order they were made, even
## several in one millisecond or when the clock steps backwards.
##
## ```roc
## ids = UuidV7.new(Rng.from_os!()?)
## now = Utc.to_millis_since_epoch(Utc.now!()).to_u64_wrap()
## (id, ids2) = ids.next(now)
## ```
##
## Within a millisecond the 12 bits after the time are a counter (RFC 9562
## §6.2, method 1). Each new millisecond starts it at a random value below
## 2048, leaving at least 2048 more before it runs out; if it does run out,
## the stream moves its time forward one millisecond, as §6.2 allows. A clock
## that goes backwards keeps the last time. A time of 2^48 milliseconds (the
## year 10889) or later crashes.
UuidV7(r) :: { rng : r, last_ms : [Unset, At(U64)], counter : U64 }.{

	## A stream drawing from `rng`. Its first UUID starts a fresh counter.
	new : r -> UuidV7(r)
	new = |source| UuidV7.{ rng: source, last_ms: Unset, counter: 0 }

	## The next UUID for `unix_ms`, and the stream after it.
	next : UuidV7(r), U64 -> (Uuid, UuidV7(r)) where [r.next_u64 : r -> (U64, r)]
	next = |stream, now| {
		if now >= time_limit {
			crash "UuidV7.next: ${now.to_str()} ms does not fit in the 48-bit time field"
		}
		advanced =
			match stream.last_ms {
				At(last) if now <= last => {
					counter = stream.counter + 1
					if counter == counter_limit {
						if last + 1 >= time_limit {
							crash "UuidV7.next: the counter ran out at the last representable millisecond"
						}
						start(stream.rng, last + 1)
					} else {
						{ rng: stream.rng, ms: last, counter }
					}
				}
				_ => start(stream.rng, now)
			}
		(b, source) = advanced.rng.next_u64()
		(Uuid.from_parts(advanced.ms, advanced.counter, b), UuidV7.{ rng: source, last_ms: At(advanced.ms), counter: advanced.counter })
	}

	## The generator, to draw from once the stream is done.
	rng : UuidV7(r) -> r
	rng = |stream| stream.rng

	## A new millisecond: the counter starts at the top 11 bits of one word.
	start : r, U64 -> { rng : r, ms : U64, counter : U64 } where [r.next_u64 : r -> (U64, r)]
	start = |source, ms| {
		(word, after) = source.next_u64()
		{ rng: after, ms, counter: word.shr_zf_wrap(53) }
	}

	counter_limit : U64
	counter_limit = 0x1000

	time_limit : U64
	time_limit = 0x1000000000000
}

pair_of : Uuid -> { hi : U64, lo : U64 }
pair_of = |id| {
	bytes = id.to_bytes()
	{ hi: bytes.take_first(8).fold(0, |acc, b| acc.shl_wrap(8).bitwise_or(b.to_u64())), lo: bytes.drop_first(8).fold(0, |acc, b| acc.shl_wrap(8).bitwise_or(b.to_u64())) }
}

run : UuidV7(r), List(U64), List(Uuid) -> (List(Uuid), UuidV7(r)) where [r.next_u64 : r -> (U64, r)]
run = |stream, times, acc|
	match times {
		[] => (acc, stream)
		[now, .. as rest] => {
			(id, after) = stream.next(now)
			run(after, rest, acc.append(id))
		}
	}

## `count` UUIDs at the same time, folded as the vectors' checksum is.
same_ms : UuidV7(r), U64, U64, { checksum : U64, last : Uuid, increasing : Bool, index : U64, around : List({ index : U64, before : { hi : U64, lo : U64 }, after : { hi : U64, lo : U64 } }) } -> ({ checksum : U64, last : Uuid, increasing : Bool, index : U64, around : List({ index : U64, before : { hi : U64, lo : U64 }, after : { hi : U64, lo : U64 } }) }, UuidV7(r)) where [r.next_u64 : r -> (U64, r)]
same_ms = |stream, count, now, acc|
	if count == 0 {
		(acc, stream)
	} else {
		(id, after) = stream.next(now)
		p = pair_of(id)
		previous = pair_of(acc.last)
		moved = acc.index > 0 and p.hi.shr_zf_wrap(16) != previous.hi.shr_zf_wrap(16)
		same_ms(after, count - 1, now, {
			checksum: acc.checksum.times_wrap(31).plus_wrap(p.hi).times_wrap(31).plus_wrap(p.lo),
			last: id,
			increasing: acc.increasing and (acc.index == 0 or id > acc.last),
			index: acc.index + 1,
			around: if moved { acc.around.append({ index: acc.index, before: previous, after: p }) } else { acc.around },
		})
	}

chacha : () -> Rng
chacha = || Rng.from_words((RngVectors.chacha_streams.get(1) ?? crash "chacha stream 1").seed)

fast : () -> FastRng
fast = || FastRng.from_words((RngVectors.xoshiro_streams.get(1) ?? crash "xoshiro stream 1").seed) ?? crash "xoshiro seed 1 is not zero"

# A first call at 0 seeds; repeats count up; a clock going backwards keeps the
# last time.
expect
	RngVectors.uuid_v7_streams.all(|v|
		match v.source {
			Chacha => {
				(ids, after) = run(UuidV7.new(chacha()), v.times, [])
				ids.map(pair_of) == v.uuids and after.rng().next_u64().0 == v.next
			}
			Fast => {
				(ids, after) = run(UuidV7.new(fast()), v.times, [])
				ids.map(pair_of) == v.uuids and after.rng().next_u64().0 == v.next
			}
		})

expect {
	(ids, _) = run(UuidV7.new(chacha()), [0, 0, 0, 5, 5, 3, 3, 9, 20, 20], [])
	pairs = ids.drop_last(1).map_with_index(|a, i| (a, ids.get(i + 1) ?? Uuid.nil))
	pairs.all(|(a, b)| a < b) and ids.all(|id| id.version() == 7)
}

# The counter runs out within one millisecond and the time moves forward.
expect {
	v = RngVectors.uuid_v7_overflow
	start = { checksum: 0, last: Uuid.nil, increasing: True, index: 0, around: [] }
	(done, after) = same_ms(UuidV7.new(fast()), v.calls, v.now, start)
	done.checksum == v.checksum and pair_of(done.last) == v.last and done.increasing and done.around == v.around and after.rng().next_u64().0 == v.next
}
