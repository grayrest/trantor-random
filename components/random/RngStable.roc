import Rng
import FastRng
import RngDraw
import Weights
import Uuid
import UuidV7

## The stability promise, written out by hand.
##
## Every value here was copied once from outputs already checked against the
## references, and no tool rewrites this file. `RngVectors` is regenerated from
## a model; if the model and the code ever changed together, those checks
## would still pass, and these would not. A failure here means a 1.x release
## would change what a saved seed or saved state produces: that is a major
## version, not a fix.
RngStable :: [].{

	chacha_seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }
	chacha_seed = { w0: 0xee9c49f7a55300b, w1: 0x3611ecc7a27d5833, w2: 0x5e3914efcaa5805b, w3: 0x86613c17f2cda883 }

	fast_seed : { w0 : U64, w1 : U64, w2 : U64, w3 : U64 }
	fast_seed = { w0: 0x0123456789abcdef, w1: 0xfedcba9876543210, w2: 0x0f1e2d3c4b5a6978, w3: 0x8796a5b4c3d2e1f0 }

	rng : () -> Rng
	rng = || Rng.from_words(chacha_seed)

	fast : () -> FastRng
	fast = || FastRng.from_words(fast_seed) ?? crash "the stable xoshiro seed is not zero"

	## The last of `count` UUIDs made at `now`, and the stream after them.
	uuids_at : UuidV7(r), U64, U64, Uuid -> (Uuid, UuidV7(r)) where [r.next_u64 : r -> (U64, r)]
	uuids_at = |stream, count, now, last|
		if count == 0 {
			(last, stream)
		} else {
			(id, after) = stream.next(now)
			uuids_at(after, count - 1, now, id)
		}

	skip : Rng, U64 -> Rng
	skip = |r, count|
		if count == 0 {
			r
		} else {
			skip(r.next_u64().1, count - 1)
		}
}

expect {
	(a, r1) = RngStable.rng().next_u64()
	(b, r2) = r1.next_u64()
	(c, _) = r2.next_u64()
	[a, b, c] == [0xda12f3b134ae6068, 0x9ababa667e46c212, 0x68e20069c6be35ec]
}

# Across the first reseed: words 124 to 127.
expect {
	r = RngStable.skip(RngStable.rng(), 124)
	(a, r1) = r.next_u64()
	(b, r2) = r1.next_u64()
	(c, r3) = r2.next_u64()
	(d, _) = r3.next_u64()
	[a, b, c, d] == [0x12543bcbe2c24aed, 0x929b94072b1a199c, 0xeb8a1fed8ddd0a60, 0x3409ce60aed86598]
}

expect {
	(a, r1) = FastRng.from_u64(42).next_u64()
	(b, r2) = r1.next_u64()
	(c, _) = r2.next_u64()
	[a, b, c] == [0xd0764d4f4476689f, 0x519e4174576f3791, 0xfbe07cfb0c24ed8c]
}

# The saved state 96 words into the stream.
expect
	RngStable.skip(Rng.from_words({ w0: 0x1cd2883ef4aa6016, w1: 0x6c22d88e44fab066, w2: 0xbc7228de944a00b6, w3: 0xcc2782ee49a5006 }), 96).to_bytes()
	== [
		0x63, 0x68, 0x61, 0x63, 0x68, 0x61, 0x38, 0x3a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60,
		0x16, 0x60, 0xaa, 0xf4, 0x3e, 0x88, 0xd2, 0x1c, 0x66, 0xb0, 0xfa, 0x44, 0x8e, 0xd8, 0x22, 0x6c,
		0xb6, 0x00, 0x4a, 0x94, 0xde, 0x28, 0x72, 0xbc, 0x06, 0x50, 0x9a, 0xe4, 0x2e, 0x78, 0xc2, 0x0c,
	]

# One output of each draw on each generator, from a fresh generator.
expect RngStable.rng().below(6).0 == 5 and RngStable.fast().below(6).0 == 3
expect RngStable.rng().between_i64(-3, 3).0 == 2 and RngStable.fast().between_i64(-3, 3).0 == 0
expect RngStable.rng().f64().0 == F64.from_bits(0x3feb425e762695cc) and RngStable.fast().f64().0 == F64.from_bits(0x3fe1e94097ef2e05)
expect RngStable.rng().between_f64(1.0, 3.0).0 == F64.from_bits(0x4005a12f3b134ae6) and RngStable.fast().between_f64(1.0, 3.0).0 == F64.from_bits(0x4000f4a04bf79702)
expect RngStable.rng().bytes(5).0 == [0x68, 0x60, 0xae, 0x34, 0xb1] and RngStable.fast().bytes(5).0 == [0xe4, 0x2a, 0x70, 0x79, 0xbf]
expect RngStable.rng().shuffle([1, 11, 21, 31, 41]).0 == [31, 1, 11, 21, 41] and RngStable.fast().shuffle([1, 11, 21, 31, 41]).0 == [11, 31, 41, 1, 21]
expect RngStable.rng().sample([1, 11, 21, 31, 41], 2).0 == [41, 31] and RngStable.fast().sample([1, 11, 21, 31, 41], 2).0 == [21, 11]
expect RngStable.rng().u32().0 == 3658675121 and RngStable.fast().u32().0 == 2403992767
expect !RngStable.rng().chance(0.5).0 and !RngStable.fast().chance(0.5).0

expect
	match Weights.from_list([(0, 1), (1, 2), (2, 3), (3, 4)]) {
		Ok(weights) => RngStable.rng().pick(weights).0 == 3 and RngDraw.pick(RngStable.fast(), weights).0 == 2
		Err(_) => False
	}

expect RngStable.rng().u64().0 == 0xda12f3b134ae6068 and RngStable.fast().u64().0 == 0x8f4a04bf79702ae4
expect RngStable.rng().u8().0 == 218 and RngStable.fast().u8().0 == 143
expect RngStable.rng().bool().0 and RngStable.rng().u64().1.bool().0 and RngStable.fast().bool().0 and !RngStable.fast().u64().1.bool().0
expect RngStable.rng().between_u64(1, 6).0 == 6 and RngStable.fast().between_u64(6, 1).0 == 4
expect RngStable.rng().between_u64(0, U64.highest).0 == 0xda12f3b134ae6068 and RngStable.fast().between_u64(U64.highest, 0).0 == 0x8f4a04bf79702ae4
expect RngStable.rng().choose([1, 11, 21, 31, 41, 51, 61]).0 == Ok(51) and RngStable.fast().choose([1, 11, 21, 31, 41, 51, 61]).0 == Ok(31)

expect
	match RngStable.fast().choose([]) {
		(Err(ListWasEmpty), rest) => rest.u64().0 == 0x8f4a04bf79702ae4
		_ => False
	}

# Seeding from a U64: SplitMix64 then ChaCha8, taken from a separate SplitMix64
# and the reference ChaCha8, not from this package.
expect {
	(a, r1) = Rng.from_u64(0).next_u64()
	(b, r2) = r1.next_u64()
	(c, _) = r2.next_u64()
	[a, b, c] == [0x337f36e3cc1e6d83, 0xebd687abba8c5e71, 0xde1181bd8c746cc5]
}

expect {
	(a, r1) = Rng.from_u64(7).next_u64()
	(b, r2) = r1.next_u64()
	(c, _) = r2.next_u64()
	[a, b, c] == [0xf1c03d9cef11d9ad, 0x721f2a56008ff7a9, 0x1553c29b33667332]
}

# Fork: `(child, parent)` on both generators.
expect {
	(child, parent) = RngStable.rng().fork()
	child.u64().0 == 0xf23199058c7ed3e7 and parent.u64().0 == 0x7919dae51a144119
}

expect {
	(child, parent) = RngStable.fast().fork()
	child.u64().0 == 0x8f4a04bf79702ae4 and parent.u64().0 == 0xee37dec04ccae38d
}

expect
	RngStable.fast().to_bytes()
	== [
		0xef, 0xcd, 0xab, 0x89, 0x67, 0x45, 0x23, 0x01, 0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe,
		0x78, 0x69, 0x5a, 0x4b, 0x3c, 0x2d, 0x1e, 0x0f, 0xf0, 0xe1, 0xd2, 0xc3, 0xb4, 0xa5, 0x96, 0x87,
	]

expect Uuid.v4(RngStable.rng()).0.to_str() == "da12f3b1-34ae-4068-9aba-ba667e46c212"
expect Uuid.v7(RngStable.rng(), 0).0.to_str() == "00000000-0000-7da1-a6ae-ae999f91b084"

# UuidV7: a fresh counter, two increments, then a new millisecond.
expect {
	(a, s1) = UuidV7.new(RngStable.rng()).next(0)
	(b, s2) = s1.next(0)
	(c, s3) = s2.next(0)
	(d, _) = s3.next(5)
	[a, b, c, d].map(Uuid.to_str) == [
		"00000000-0000-76d0-a6ae-ae999f91b084",
		"00000000-0000-76d1-9a38-801a71af8d7b",
		"00000000-0000-76d2-89c9-d73dd6c37f93",
		"00000000-0005-73c8-825a-e136cfde7403",
	]
}

# The counter runs out on the 2951st UUID in millisecond 1000 and the time
# moves to 1001.
expect {
	(before, stream) = RngStable.uuids_at(UuidV7.new(RngStable.fast()), 2950, 1000, Uuid.nil)
	(after, _) = stream.next(1000)
	before.to_str() == "00000000-03e8-7fff-8799-290d069eb7b0" and after.to_str() == "00000000-03e9-75ea-b551-ea8a00b60b76"
}
