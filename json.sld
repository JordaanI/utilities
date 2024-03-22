(define-library (json)
  (export
    table->json-string
    is-json-string?
    json-string->table)
  (import
    (gambit))
  (begin
    (include "json.scm")))