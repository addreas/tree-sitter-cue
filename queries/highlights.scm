
(identifier) @variable

[
  "len"
  "close"
  "or"
  "quo"
  "rem"
  "div"
  "mod"
] @variable.builtin

[
  (null)
  (bottom)
  (true)
  (false)
  "int"
  "float"
  "string"
  "bytes"
  "bool"
  "number"
  "uint"
  "uint8"
  "int8"
  "uint16"
  "int16"
  "rune"
  "uint32"
  "int32"
  "uint64"
  "int64"
  "uint128"
  "int128"
  "float32"
  "float6"
] @constant.builtin

(comment) @comment

(string) @string

(number) @number

(interpolation
  "\\" "(" @punctuation.special
  ")" @punctuation.special) @embedded

[
  "+"
  "&&"
  "=="
  "<"
  "="
  "-"
  "||"
  "!="
  ">"
  "*"
  "&"
  "=~"
  "<="
  "/"
  "|"
  "!~"
  ">="
  "!"
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
]  @punctuation.bracket


[
  "import"
  "package"
] @keyword.control.import

[
  ":"
  "."
  ","
] @punctuation.delimiter

[
  "for"
  "in"
  "if"
  "let"
] @keyword
