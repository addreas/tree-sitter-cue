(struct) @local.scope
(list) @local.scope

(let_clause (identifier) @local.definition)
(for_clause (identifier) @local.definition)
(for_clause
  (identifier) @local.definition
  (identifier) @local.definition)

(identifier) @local.reference