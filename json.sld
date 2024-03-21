(define-library (json)
  (export
    table->json-list)
  (import
    (gambit))
  (begin
    (include "json.scm")))