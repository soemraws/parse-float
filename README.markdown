A function to parse a floating point value in a string.

parse-float _string_ &key _start_ _end_ _radix_ _junk-allowed_ _decimal-character_ _exponent-character_ _type_ => _float_, _pos_

Arguments and Values:
---------------------

_string_---a [string](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#string "CLHS").

_start_, _end_---[bounding index designators](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index_designator "CLHS") of _string_. The defaults for start and end are 0 and [nil](http://www.lispworks.com/documentation/HyperSpec/Body/a_nil.htm#nil "CLHS"), respectively.

_radix_---a [radix](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#radix "CLHS"). The default is 10.

_junk-allowed_---a [generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean "CLHS"). The default is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "CLHS").

_decimal-character_---a [character](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "CLHS") separating the integer and decimal parts. The default is #\..

_exponent-character_---the exponentiation [character](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "CLHS") (case insensitive). The default is #\e.

_type_---a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS") [type specifier](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type_specifier "CLHS"). The default is [*READ-DEFAULT-FLOAT-FORMAT*](http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_def.htm "CLHS").

_float_---a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS"), depending on _type_, or [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "CLHS").

_pos_---a [bounding index](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index "CLHS") of _string_.

Description:
------------

**parse-float** parses a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS") in the specified _radix_ from the substring of _string_ delimited by _start_ and _end_ into a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS") of the given _type_.

**parse-float** expects an optional sign (+ or -) followed by a a non-empty sequence of digits to be interpreted in the specified _radix_, optionally followed by _decimal-character_, a sequence of digits, _exponent-character_, a sign (+ or -) and a sequence of digits. Optional leading and trailing [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "CLHS") is ignored.

**parse-float** does not recognize the syntactic radix-specifier prefixes `#O`, `#B`, `#X`, and `#nR`, nor does it recognize the exponent if _radix_ is not 10. 

If _junk-allowed_ is _false_, an error of [type](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type "CLHS") [parse-error](http://www.lispworks.com/documentation/HyperSpec/Body/e_parse_.htm#parse-error "CLHS") is signaled if substring does not consist entirely of the representation of a signed [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS"), possibly surrounded on either side by [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "CLHS") [characters](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "CLHS").

The first [value](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_v.htm#value "CLHS") returned is either the [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS") that was parsed, or else **nil** if no syntactically correct [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS") was seen but _junk-allowed_ was [true](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#true "CLHS").

The second [value](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_v.htm#value "CLHS") is either the index into the [string](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#string "CLHS") of the delimiter that terminated the parse, or the upper [bounding index](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index "CLHS") of the substring if the parse terminated at the end of the substring (as is always the case if junk-allowed is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "CLHS")).

Examples:
---------

 (parse-float "123") =>  123.0, 3
 (parse-float "123.1" :start 1 :radix 5 :type 'double-float) =>  13.2d0, 5
 (parse-float "123,0D2" :decimal-character #\, :exponent-character #\d :type 'single-float) =>  12300.0, 7
 (parse-float "no-integer" :junk-allowed t) =>  NIL, 0

Side Effects:
-------------

None.

Affected By:
------------

None.

Exceptional Situations:
-----------------------

If _junk-allowed_ is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "CLHS"), an error is signaled if substring does not consist entirely of the representation of a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "CLHS"), possibly surrounded on either side by [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "CLHS") [characters](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "CLHS").

See Also:
---------

[parse-integer](http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm "CLHS"), [parse-number] (https://github.com/sharplispers/parse-number "Github")
