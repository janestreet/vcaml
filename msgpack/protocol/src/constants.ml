(* nil format *)
let nil = '\xc0'

(* bool format family *)
let false_ = '\xc2'
let true_ = '\xc3'

(* The ``fix'' variants of each family encode some information about the item (typically
   the size) in the header byte itself. For example, a header byte of [1110 1111] matches
   the mask [111X XXXX], which means ``negative fixint'', and represents a value of -15,
   as the remaining bits are [0 1111] = 15.

   See [https://github.com/msgpack/msgpack/blob/master/spec.md#formats] for the technical
   details.
*)

(* int format family *)
(* The format for positive fixints is [0XXX YYYY], so the correct way to test a byte
   against this mask is to check that [ value & unmask == value ].
*)
let positive_fixint_unmask = 0x7F
let negative_fixint_mask = 0xE0
let uint8_header = '\xcc'
let uint16_header = '\xcd'
let uint32_header = '\xce'
let uint64_header = '\xcf'
let int8_header = '\xd0'
let int16_header = '\xd1'
let int32_header = '\xd2'
let int64_header = '\xd3'

(* float format family *)
let float32_header = '\xca'
let float64_header = '\xcb'

(* string format family *)
let fixstr_mask = 0xA0
let str8_header = '\xd9'
let str16_header = '\xda'
let str32_header = '\xdb'

(* binary format family *)
let bin8_header = '\xc4'
let bin16_header = '\xc5'
let bin32_header = '\xc6'

(* array format family *)
let fixarray_mask = 0x90
let array16_header = '\xdc'
let array32_header = '\xdd'

(* map format family *)
let fixmap_mask = 0x80
let map16_header = '\xde'
let map32_header = '\xdf'

(* ext format family *)
let fixext1_header = '\xd4'
let fixext2_header = '\xd5'
let fixext4_header = '\xd6'
let fixext8_header = '\xd7'
let fixext16_header = '\xd8'
let ext8_header = '\xc7'
let ext16_header = '\xc8'
let ext32_header = '\xc9'
