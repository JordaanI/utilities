(define-library (string)
		(export
			split-string
			string-contain?
			substring
			strip-char)

		(import
			(gambit))

		(begin
			(include "string.scm")))