module.exports = grammar({
  name: 'cue',

  extras: $ => [$.comment, /\s/],

  supertypes: $ => [],

  inline: $ => [
    $._declaration,
    $._attr_tokens,
    $._element_list,
    $._operand,
    $._operand_name,
    $._literal,
    $._qualified_ident,
    $._argument,
    $._rel_op,
    $._clauses,
    $._start_clause,
  ],

  word: $ => $.identifier,

  rules: {
    // SourceFile = { attribute "," } [ PackageClause "," ] { ImportDecl "," } { Declaration "," } .
    source_file: $ => seq(
      repeat($.attribute),
      optional($.package_clause),
      repeat($.import_decl),
      repeat($._declaration)
    ),

    // PackageClause  = "package" PackageName .
    package_clause: $ => seq("package", $.package_name),
    // PackageName    = identifier .
    package_name:   $ => $.identifier,

    // ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec "," } ")" ) .
    import_decl:     $ => seq("import", choice($.import_spec, seq("(", repeat(seq($.import_spec, optional(","))), ")"))), // TODO: EBNF doens not contain optional comma
    // ImportSpec       = [ PackageName ] ImportPath .
    import_spec:     $ => seq(optional($.package_name), $.import_path),
    // ImportLocation   = { unicode_value } .
    import_location: $ => repeat1(choice(/[^\n"]/, $._unicode_value)),
    // ImportPath       = `"` ImportLocation [ ":" identifier ] `"` .    
    import_path:     $ => seq(`"`, $.import_location, optional(seq(":", $.identifier)), `"`),

    // StructLit       = "{" { Declaration "," } "}" .
    struct:       $ => seq("{", repeat(seq($._declaration, optional(","))), "}"),
    // Declaration     = Field | Ellipsis | Embedding | LetClause | attribute .
    _declaration: $ => prec(1, choice( // TODO: verify
      $.field,
      $.ellipsis,
      $._embedding,
      $.let_clause,
      $.attribute
    )),
    // Ellipsis        = "..." [ Expression ] .
    ellipsis:     $ => prec.left(seq("...", optional($._expression))), // TODO: verify
    // Embedding       = Comprehension | AliasExpr .
    _embedding:   $ => prec.left(choice($.comprehension, $._alias_expr)), // TODO: verify
    // Field           = Label ":" { Label ":" } AliasExpr { attribute } .
    field:        $ => prec.left(seq($.label, ":", repeat(seq($.label, ":")), $._alias_expr, repeat($.attribute))), // TODO: verify
    // Label           = [ identifier "=" ] LabelExpr .
    label:        $ => seq(optional(seq($.identifier, "=")), $._label_expr),
    // LabelExpr       = LabelName [ "?" ] | "[" AliasExpr "]" .
    _label_expr:  $ => choice(
      seq($.label_name, optional("?")),
      seq("[", $._alias_expr, "]")
    ),
    // LabelName       = identifier | simple_string_lit  .
    label_name: $ => choice($.identifier, $.simple_string),

    // attribute       = "@" identifier "(" attr_tokens ")" .
    attribute:    $ => seq("@", $.identifier, "(", $._attr_tokens, ")"),
    // attr_tokens     = { attr_token |
    //                     "(" attr_tokens ")" |
    //                     "[" attr_tokens "]" |
    //                     "{" attr_tokens "}" } .
    _attr_tokens: $ => repeat1(choice(
       $._attr_token,
      seq("(", $._attr_token, ")"),
      seq("[", $._attr_token, "]"),
      seq("{", $._attr_token, "}"),
    )),
    // attr_token      = /* any token except '(', ')', '[', ']', '{', or '}' */
    _attr_token:  $ => /[^\(\)\{\}\[\]]/,

    // AliasExpr  = [ identifier "=" ] Expression .
    _alias_expr:  $ => prec.right(seq(optional(seq($.identifier, "=")), $._expression)),

    // ListLit       = "[" [ ElementList [ "," ] ] "]" .
    list:          $ => seq("[", optional(seq($._element_list, optional(","))), "]"),
    // ElementList   = Ellipsis | Embedding { "," Embedding } [ "," Ellipsis ] .
    _element_list: $ => prec.right(choice( // TODO: verify
      $.ellipsis,
      seq(commaSep1($._embedding), optional(seq(",", $.ellipsis))),
    )),

    // Operand     = Literal | OperandName | "(" Expression ")" .
    _operand:      $ => choice($._literal, $._operand_name, seq("(", $._expression, ")")),
    // OperandName = identifier | QualifiedIdent .
    _operand_name: $ => prec(1, choice($.identifier, $._qualified_ident)), // TODO: verify
    // Literal     = BasicLit | ListLit | StructLit .
    // BasicLit    = int_lit | float_lit | string_lit | null_lit | bool_lit | bottom_lit .
    _literal:      $ => choice(
      $.number,
      $.string,
      $.bytes,
      $.null,
      $._bool_lit,
      $.bottom,
      $.list,
      $.struct
    ),

    // QualifiedIdent = PackageName "." identifier .
    _qualified_ident: $ => seq($.package_name, ".", $.identifier),

    // PrimaryExpr =
    //   Operand |
    //   PrimaryExpr Selector |
    //   PrimaryExpr Index |
    //   PrimaryExpr Slice |
    //   PrimaryExpr Arguments .
    _primary_expr: $ => choice(
      $._operand,
      seq($._primary_expr, $.selector),
      seq($._primary_expr, $.index),
      seq($._primary_expr, $.slice),
      seq($._primary_expr, $.arguments),
    ),

    // Selector       = "." (identifier | simple_string_lit) .
    selector:  $ => seq(".", choice($.identifier, $.simple_string)),
    // Index          = "[" Expression "]" .
    index:     $ => seq("[", $._expression, "]"),
    // TODO: missing in spec.md
    slice:     $ => seq("[", $._expression, ":", $._expression, "]"),
    // Argument       = Expression .
    _argument: $ => $._expression,
    // Arguments      = "(" [ ( Argument { "," Argument } ) [ "," ] ] ")" .
    arguments: $ => seq("(", commaSep($._argument), ")"),

    // Expression = UnaryExpr | Expression binary_op Expression .
    _expression: $ => prec.left(choice($._unary_expr, seq($._expression, $._binary_op, $._expression))),
    // UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
    _unary_expr: $ => prec.right(choice($._primary_expr, seq($._unary_op, $._unary_expr))), // TODO: verify

    // binary_op  = "|" | "&" | "||" | "&&" | "==" | rel_op | add_op | mul_op  .
    // add_op     = "+" | "-" .
    // mul_op     = "*" | "/" .

    // Precedence    Operator
    //     7             *  /
    //     6             +  -
    //     5             ==  !=  <  <=  >  >= =~ !~
    //     4             &&
    //     3             ||
    //     2             &
    //     1             |
    _binary_op: $ => choice(
      prec.left(7, choice("*", token.immediate("/"))),
      prec.left(6, choice("+", "-")),
      prec.left(5, $._rel_op),
      prec.left(4, "&&"),
      prec.left(3, "||"),
      prec.left(2, "&"),
      prec.left(1, "|"),
    ),
    // rel_op     = "!=" | "<" | "<=" | ">" | ">=" | "=~" | "!~" .
    _rel_op:    $ => choice("==", "!=", "<", "<=", ">", ">=", "=~", "!~"),
    // unary_op   = "+" | "-" | "!" | "*" | rel_op .
    _unary_op:  $ => choice("+", "-", "!", "*", $._rel_op),

    // Comprehension       = Clauses StructLit .
    comprehension: $ => seq($._clauses, $.struct),

    // Clauses             = StartClause { [ "," ] Clause } .
    _clauses:      $ => seq($._start_clause, repeat(seq(optional(","), $.clause))),
    // StartClause         = ForClause | GuardClause .
    _start_clause: $ => choice($.for_clause, $.guard_clause),
    // Clause              = StartClause | LetClause .
    clause:        $ => choice($._start_clause, $.let_clause),
    // ForClause           = "for" identifier [ "," identifier ] "in" Expression .
    for_clause:    $ => seq("for", $.identifier, optional(seq(",", $.identifier)), "in", $._expression),
    // GuardClause         = "if" Expression .
    guard_clause:  $ => seq("if", $._expression),
    // LetClause           = "let" identifier "=" Expression .
    let_clause:    $ => prec.right(seq("let", $.identifier, "=", $._expression)), // TODO: verify

    _unicode_value: $ => {
      let escaped_char     = seq("\\", repeat(`#`), /(\'|\"|\\|\/|a|b|f|n|r|t|v)/)
      let little_u_value   = seq("\\", repeat(`#`), "u", /[0-9a-fA-F]{4}/)
      let big_u_value      = seq("\\", repeat(`#`), "U", /[0-9a-fA-F]{8}/)

      return token.immediate(choice(/[^\n]/, little_u_value, big_u_value, escaped_char))
    },

    _newline: $ => token.immediate("\n"),

    interpolation: $ => seq("\\", repeat(`#`), "(", $._expression, ")"),

    simple_string: $ => seq(`"`, repeat(choice($._unicode_value, $.interpolation)), `"`),

    string: $ => {
      let multiline_string = seq(
        `"""`,
        $._newline,
        repeat(choice($._unicode_value, $.interpolation, $._newline)),
        `"""`
      )

      return choice(
        alias($.simple_string, "simple"),
        multiline_string,
        seq(`#`, $.string, `#`)
      )
    },

    bytes: $ => {
      let octal_byte_value = token.immediate(seq("\\", repeat(`#`), /[0-7]{3}/))
      let hex_byte_value   = token.immediate(seq("\\", repeat(`#`), "x", /[0-9a-fA-F]{2}/))

      let byte_value = token.immediate(choice(octal_byte_value, hex_byte_value))

      let simple_bytes = seq(`'`, repeat(choice($._unicode_value, $.interpolation, byte_value)), `'`)
      let multiline_bytes = seq(
        `'''`,
        $._newline,
        repeat(choice($._unicode_value, $.interpolation, byte_value, $._newline)),
        `'''`
      )

      return choice(
        simple_bytes,
        multiline_bytes,
        seq(`#`, $.bytes, `#`)
      )
    },

    number: $ => {
      const hex_literal = seq(choice('0x', '0X'), /[\da-fA-F]+/)

      const decimal_digits = /\d+/
      const signed_integer = seq(optional(choice('-','+')), decimal_digits)
      const exponent_part = seq(choice('e', 'E'), signed_integer)

      const binary_literal = seq(choice('0b', '0B'), /[0-1]+/)

      const octal_literal = seq(choice('0o', '0O'), /[0-7]+/)

      const decimal_integer_literal = seq(
        optional(choice('-','+')),
        choice(
          '0',
          seq(/[1-9]/, optional(decimal_digits))
        )
      )
      const multiplier = /(K|M|G|T|P)i?/

      const decimal_literal = choice(
        seq(decimal_integer_literal, '.', optional(decimal_digits), optional(exponent_part), optional(multiplier)),
        seq('.', decimal_digits, optional(exponent_part), optional(multiplier)),
        seq(decimal_integer_literal, optional(exponent_part), optional(multiplier))
      )

      return token(choice(
        hex_literal,
        decimal_literal,
        binary_literal,
        octal_literal
      ))
    },

    identifier: $ => /(#|_#)?\p{L}(\p{L}|\p{Nd})*/,

    _bool_lit: $ => choice($.true, $.false),

    bottom: $ => "_|_",
    true: $ => "true",
    false: $ => "false",
    null: $ => "null",

    // https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js    
    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ => token(prec(-1, choice(seq('//', /.*/, optional('\n'))))),

    _kw_builtin_function: $ => choice(
      "len",
      "close",
      "and",
      "or",
      "quo",
      "rem",
      "div",
      "mod",
    ),
    _kw_derived_types: $ => choice(
      "int",
      "float",
      "string",
      "bytes",
      "bool",
      "number",
      "uint",
      "uint8",
      "int8",
      "uint16",
      "int16",
      "rune",
      "uint32",
      "int32",
      "uint64",
      "int64",
      "uint128",
      "int128",
      "float32",
      "float6",
    ),
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","))
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}
