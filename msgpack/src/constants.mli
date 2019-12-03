(* nil format *)
val nil : char

(* bool format family *)
val true_ : char
val false_ : char

(* int format family *)
val positive_fixint_unmask : int
val negative_fixint_mask : int
val uint8_header : char
val uint16_header : char
val uint32_header : char
val uint64_header : char
val int8_header : char
val int16_header : char
val int32_header : char
val int64_header : char

(* float format family *)
val float32_header : char
val float64_header : char

(* string format family *)
val fixstr_mask : int
val str8_header : char
val str16_header : char
val str32_header : char

(* binary format family *)
val bin8_header : char
val bin16_header : char
val bin32_header : char

(* array format family *)
val fixarray_mask : int
val array16_header : char
val array32_header : char

(* map format family *)
val fixmap_mask : int
val map16_header : char
val map32_header : char

(* ext format family *)
val fixext1_header : char
val fixext2_header : char
val fixext4_header : char
val fixext8_header : char
val fixext16_header : char
val ext8_header : char
val ext16_header : char
val ext32_header : char
