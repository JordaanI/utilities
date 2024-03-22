(define-library (json)
  (export
    table->json-string
    is-json-string?
    json-string->table)
  (import
    (gambit)
    (github.com/JordaanI/utilities utilities))
  (begin
    (include "json.scm")))