(define-library (json)
  (export
    table->json-string
    is-json-string?
    is-json-list?
    json-string->table)
  (import
    (gambit)
    (.. string))
  (begin
    (include "json.scm")))