import Rng
import FastRng
import RngVectors

## A 128-bit UUID (RFC 9562).
##
## `v4` is 122 random bits; `v7` puts a millisecond Unix time first so UUIDs
## sort by when they were made (`UuidV7` keeps that order within a
## millisecond). Both draw from a generator: use `Rng`, whose output cannot be
## predicted, for UUIDs anyone might want to guess.
##
## ```roc
## rng = Rng.from_os!()?
## (id, rng2) = Uuid.v4(rng)
## Stdout.line!(id.to_str())?
## ```
##
## UUIDs compare and sort in RFC byte order, hash, and encode as their string
## form, so they work as `Dict` keys and in JSON. On the current compiler a
## `Uuid` decodes through `Json.parse` on its own but not as a record field.
Uuid :: { hi : U64, lo : U64 }.{

	## `00000000-0000-0000-0000-000000000000`.
	nil : Uuid
	nil = Uuid.{ hi: 0, lo: 0 }

	## `ffffffff-ffff-ffff-ffff-ffffffffffff`.
	max : Uuid
	max = Uuid.{ hi: U64.highest, lo: U64.highest }

	## A random (version 4) UUID: two words from the generator with the
	## version and variant bits set.
	v4 : r -> (Uuid, r) where [r.next_u64 : r -> (U64, r)]
	v4 = |rng| {
		(high, r1) = rng.next_u64()
		(low, r2) = r1.next_u64()
		(Uuid.{ hi: high.bitwise_and(0xffffffffffff0fff).bitwise_or(0x4000), lo: with_variant(low) }, r2)
	}

	## A time-ordered (version 7) UUID for `unix_ms` milliseconds since the Unix
	## epoch. Only the low 48 bits of the time fit, which lasts until the year
	## 10889. Several in the same millisecond come out in random order; use
	## `UuidV7` when they must sort in the order they were made.
	v7 : r, U64 -> (Uuid, r) where [r.next_u64 : r -> (U64, r)]
	v7 = |rng, unix_ms| {
		(a, r1) = rng.next_u64()
		(b, r2) = r1.next_u64()
		(from_parts(unix_ms.bitwise_and(time_mask), a.shr_zf_wrap(52), b), r2)
	}

	## The layout both v7 forms share: 48-bit time, version 7, 12 bits of
	## `rand_a`, the variant, then the top 62 bits of `b`.
	##
	## An app can bring its own time and counter here, so each argument is
	## trimmed to its field rather than refused: `unix_ms` keeps its low 48
	## bits and `rand_a` its low 12, the same trimming `v7` does. An unmasked
	## `rand_a` would spill into the version nibble beside it, and the result
	## would not be a v7 at all.
	from_parts : U64, U64, U64 -> Uuid
	from_parts = |unix_ms, rand_a, b| Uuid.{ hi: unix_ms.bitwise_and(time_mask).shl_wrap(16).bitwise_or(0x7000).bitwise_or(rand_a.bitwise_and(rand_a_mask)), lo: with_variant(b.shr_zf_wrap(2)) }

	## The version number in the UUID's version field (4 for `v4`, 7 for `v7`).
	version : Uuid -> U8
	version = |uuid| uuid.hi.shr_zf_wrap(12).bitwise_and(0xf).to_u8_wrap()

	## Lowercase hex in the `8-4-4-4-12` form.
	to_str : Uuid -> Str
	to_str = |uuid| {
		hex = hex_digits(uuid.hi, 16, []).concat(hex_digits(uuid.lo, 16, []))
		grouped = [
			hex.sublist({ start: 0, len: 8 }),
			hex.sublist({ start: 8, len: 4 }),
			hex.sublist({ start: 12, len: 4 }),
			hex.sublist({ start: 16, len: 4 }),
			hex.sublist({ start: 20, len: 12 }),
		].intersperse(['-']).join()
		Str.from_utf8_lossy(grouped)
	}

	## Parses the `8-4-4-4-12` form, in either case. Braces, a `urn:uuid:`
	## prefix and the unhyphenated form are refused.
	from_str : Str -> Try(Uuid, [InvalidUuid])
	from_str = |text| {
		chars = Str.to_utf8(text)
		if chars.len() != text_len {
			Err(InvalidUuid)
		} else {
			start : Try({ hi : U64, lo : U64, digits : U64 }, [InvalidUuid])
			start = Ok({ hi: 0, lo: 0, digits: 0 })
			parsed = chars.fold_with_index_until(start, |acc, byte, index|
				match acc {
					Err(_) => Break(acc)
					Ok(so_far) =>
						if hyphen_at(index) {
							if byte == '-' { Continue(acc) } else { Break(Err(InvalidUuid)) }
						} else {
							match hex_value(byte) {
								Err(_) => Break(Err(InvalidUuid))
								Ok(nibble) =>
									if so_far.digits < 16 {
										Continue(Ok({ ..so_far, hi: so_far.hi.shl_wrap(4).bitwise_or(nibble), digits: so_far.digits + 1 }))
									} else {
										Continue(Ok({ ..so_far, lo: so_far.lo.shl_wrap(4).bitwise_or(nibble), digits: so_far.digits + 1 }))
									}
							}
						}
				})
			match parsed {
				Ok(done) => Ok(Uuid.{ hi: done.hi, lo: done.lo })
				Err(_) => Err(InvalidUuid)
			}
		}
	}

	## The 16 bytes in RFC order (big-endian).
	to_bytes : Uuid -> List(U8)
	to_bytes = |uuid| be_bytes(uuid.hi).concat(be_bytes(uuid.lo))

	## The UUID from 16 bytes in RFC order.
	from_bytes : List(U8) -> Try(Uuid, [InvalidUuid])
	from_bytes = |data|
		if data.len() != 16 {
			Err(InvalidUuid)
		} else {
			Ok(Uuid.{ hi: be_u64(data.take_first(8)), lo: be_u64(data.drop_first(8)) })
		}

	is_eq : Uuid, Uuid -> Bool
	is_eq = |a, b| a.hi == b.hi and a.lo == b.lo

	is_lt : Uuid, Uuid -> Bool
	is_lt = |a, b| a.hi < b.hi or (a.hi == b.hi and a.lo < b.lo)

	is_lte : Uuid, Uuid -> Bool
	is_lte = |a, b| !is_lt(b, a)

	is_gt : Uuid, Uuid -> Bool
	is_gt = |a, b| is_lt(b, a)

	is_gte : Uuid, Uuid -> Bool
	is_gte = |a, b| !is_lt(a, b)

	to_hash : Uuid, Hasher -> Hasher
	to_hash = |uuid, hasher| uuid.lo.to_hash(uuid.hi.to_hash(hasher))

	## Encodes as the string form, through any format.
	encoder_for : encoding -> (Uuid, state -> Try(state, err))
		where [
			encoding.encode_str : Str, state -> Try(state, err),
		]
	encoder_for = |_encoding| {
		Encoding : encoding

		|uuid, state| Encoding.encode_str(to_str(uuid), state)
	}

	## Decodes the string form, through any format.
	parser_for : encoding -> (state -> Try({ value : Uuid, rest : state }, problem))
		where [
			encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, problem),
			encoding.invalid_value : encoding, state -> problem,
		]
	parser_for = |encoding| {
		Encoding : encoding

		|state| {
			parsed = Encoding.parse_str(encoding, state)?

			match from_str(parsed.value) {
				Ok(uuid) => Ok({ value: uuid, rest: parsed.rest })
				Err(_) => Err(Encoding.invalid_value(encoding, state))
			}
		}
	}

	to_inspect : Uuid -> Str
	to_inspect = |uuid| "Uuid(${to_str(uuid)})"

	time_mask : U64
	time_mask = 0xffffffffffff

	rand_a_mask : U64
	rand_a_mask = 0xfff

	text_len : U64
	text_len = 36

	hyphen_at : U64 -> Bool
	hyphen_at = |index| index == 8 or index == 13 or index == 18 or index == 23

	with_variant : U64 -> U64
	with_variant = |low| low.bitwise_and(0x3fffffffffffffff).bitwise_or(0x8000000000000000)

	hex_value : U8 -> Try(U64, [NotHex])
	hex_value = |byte|
		if byte >= '0' and byte <= '9' {
			Ok((byte - '0').to_u64())
		} else if byte >= 'a' and byte <= 'f' {
			Ok((byte - 'a' + 10).to_u64())
		} else if byte >= 'A' and byte <= 'F' {
			Ok((byte - 'A' + 10).to_u64())
		} else {
			Err(NotHex)
		}

	## The low `count` nibbles of `word` as lowercase hex, most significant first.
	hex_digits : U64, U64, List(U8) -> List(U8)
	hex_digits = |word, count, acc|
		if count == 0 {
			acc
		} else {
			nibble = word.shr_zf_wrap(((count - 1) * 4).to_u8_wrap()).bitwise_and(0xf).to_u8_wrap()
			digit = if nibble < 10 { '0' + nibble } else { 'a' + nibble - 10 }
			hex_digits(word, count - 1, acc.append(digit))
		}

	be_bytes : U64 -> List(U8)
	be_bytes = |word| [56, 48, 40, 32, 24, 16, 8, 0].map(|shift| word.shr_zf_wrap(shift).to_u8_wrap())

	be_u64 : List(U8) -> U64
	be_u64 = |data| data.fold(0, |acc, byte| acc.shl_wrap(8).bitwise_or(byte.to_u64()))
}

of : { hi : U64, lo : U64 } -> Uuid
of = |{ hi, lo }| Uuid.{ hi, lo }

chacha : () -> Rng
chacha = || Rng.from_words((RngVectors.chacha_streams.get(1) ?? crash "chacha stream 1").seed)

fast : () -> FastRng
fast = || FastRng.from_words((RngVectors.xoshiro_streams.get(1) ?? crash "xoshiro stream 1").seed) ?? crash "xoshiro seed 1 is not zero"

v4s : r, U64, List(Uuid) -> (List(Uuid), r) where [r.next_u64 : r -> (U64, r)]
v4s = |rng, count, acc|
	if count == 0 {
		(acc, rng)
	} else {
		(id, next) = Uuid.v4(rng)
		v4s(next, count - 1, acc.append(id))
	}

v7s : r, List(U64), List(Uuid) -> (List(Uuid), r) where [r.next_u64 : r -> (U64, r)]
v7s = |rng, times, acc|
	match times {
		[] => (acc, rng)
		[now, .. as rest] => {
			(id, next) = Uuid.v7(rng, now)
			v7s(next, rest, acc.append(id))
		}
	}

# RFC 9562 Appendix A.
expect
	RngVectors.uuid_rfc_examples.all(|e|
		match Uuid.from_str(e.text) {
			Ok(id) => id == of(e.uuid) and id.version() == e.version and id.to_bytes() == e.bytes and id.to_str() == e.text
			Err(_) => False
		})

expect
	RngVectors.uuid_formats.all(|f| {
		id = of(f.uuid)
		upper = Str.from_utf8_lossy(Str.to_utf8(f.text).map(|c| if c >= 'a' and c <= 'f' { c - 32 } else { c }))
		id.to_str() == f.text
		and Uuid.from_str(f.text) == Ok(id)
		and Uuid.from_str(upper) == Ok(id)
		and id.version() == f.version
		and id.to_bytes() == f.bytes
		and Uuid.from_bytes(f.bytes) == Ok(id)
	})

expect {
	sorted = RngVectors.uuid_sorted.map(of)
	pairs = sorted.drop_last(1).map_with_index(|a, i| (a, sorted.get(i + 1) ?? Uuid.nil))
	pairs.all(|(a, b)| a < b and a <= b and b > a and b >= a and a != b)
}

expect Uuid.nil.to_str() == "00000000-0000-0000-0000-000000000000" and Uuid.max.to_str() == "ffffffff-ffff-ffff-ffff-ffffffffffff"

# Only the hyphenated 36-character form parses.
expect {
	good = "919108f7-52d1-4320-9bac-f847db4148a8"
	refused = [
		"",
		"919108f752d143209bacf847db4148a8",
		"{919108f7-52d1-4320-9bac-f847db4148a8}",
		"urn:uuid:919108f7-52d1-4320-9bac-f847db4148a8",
		"919108f7-52d1-4320-9bac-f847db4148a",
		"919108f7-52d1-4320-9bac-f847db4148a8a",
		"919108f7-52d14-320-9bac-f847db4148a8",
		"919108f7_52d1_4320_9bac_f847db4148a8",
		"919108g7-52d1-4320-9bac-f847db4148a8",
		" 919108f7-52d1-4320-9bac-f847db4148a",
	]
	Uuid.from_str(good).is_ok() and refused.all(|text| !Uuid.from_str(text).is_ok())
}

expect [List.repeat(0, 15), List.repeat(0, 17), []].all(|data| !Uuid.from_bytes(data).is_ok())

expect
	RngVectors.uuid_v4.all(|v| {
		expected = v.uuids.map(of)
		match v.source {
			Chacha => {
				(ids, rest) = v4s(chacha(), expected.len(), [])
				ids == expected and rest.next_u64().0 == v.next and ids.all(|id| id.version() == 4)
			}
			Fast => {
				(ids, rest) = v4s(fast(), expected.len(), [])
				ids == expected and rest.next_u64().0 == v.next and ids.all(|id| id.version() == 4)
			}
		}
	})

expect
	RngVectors.uuid_v7.all(|v| {
		expected = v.uuids.map(of)
		match v.source {
			Chacha => {
				(ids, rest) = v7s(chacha(), v.times, [])
				ids == expected and rest.next_u64().0 == v.next and ids.all(|id| id.version() == 7)
			}
			Fast => {
				(ids, rest) = v7s(fast(), v.times, [])
				ids == expected and rest.next_u64().0 == v.next and ids.all(|id| id.version() == 7)
			}
		}
	})

# `from_parts` is reachable from an app with a time and counter of its own, so
# each argument has to stay inside its field: 12 bits of `rand_a` next to the
# version, 48 of time next to that.
expect {
	masked = Uuid.from_parts(0x1000000000005, 0xffff, 0)
	masked.version() == 7 and masked == Uuid.from_parts(5, 0xfff, 0)
}

# A record holding a Uuid goes through JSON as the string form, both ways.
expect {
	id = Uuid.from_str("017f22e2-79b0-7cc3-98c4-dc0c0c07398f") ?? Uuid.nil
	Json.to_str({ id: id }) == "{\"id\":\"017f22e2-79b0-7cc3-98c4-dc0c0c07398f\"}"
}

expect {
	decoded : Try(Uuid, [InvalidJson(Str)])
	decoded = Json.parse("\"017F22E2-79B0-7CC3-98C4-DC0C0C07398F\"")
	match decoded {
		Ok(id) => id.to_str() == "017f22e2-79b0-7cc3-98c4-dc0c0c07398f"
		Err(_) => False
	}
}

expect {
	decoded : Try(Uuid, [InvalidJson(Str)])
	decoded = Json.parse("\"not-a-uuid\"")
	!decoded.is_ok()
}

expect {
	a = Uuid.from_str("017f22e2-79b0-7cc3-98c4-dc0c0c07398f") ?? Uuid.nil
	table = Dict.empty().insert(a, "a").insert(Uuid.max, "max")
	table.get(a) == Ok("a") and table.get(Uuid.nil) == Err(KeyNotFound)
}
