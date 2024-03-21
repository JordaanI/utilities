(define-library (json)
  (export
    table->json-string)
  (import
    (gambit))
  (begin
    (include "json.scm")))