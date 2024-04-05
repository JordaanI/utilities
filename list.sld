(define-library (list)
		(export
			flatten
			pairize
			collect
			sort)
		(import
			(gambit))
		(begin
			(include "list.scm")))