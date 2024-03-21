;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                        ;;;
;;;     __  .______     ______   .__   __.    .______    __    _______.                    ;;;
;;;    |  | |   _  \   /  __  \  |  \ |  |    |   _  \  |  |  /  _____|        _____       ;;;   
;;;    |  | |  |_)  | |  |  |  | |   \|  |    |  |_)  | |  | |  |  __      ^..^     \9     ;;;
;;;    |  | |      /  |  |  |  | |  . `  |    |   ___/  |  | |  | |_ |     (oo)_____/      ;;;
;;;    |  | |  |\  \  |  `--'  | |  |\   |    |  |      |  | |  |__| |        WW  WW       ;;;
;;;    |__| | _| `._|  \______/  |__| \__|    | _|      |__|  \______|                     ;;;
;;;                                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author: Ivan Jordaan
;; Date: 2024-03-21
;; email: ivan@axoinvent.com
;; Project: json Utilities
;;

;;;
;;;; Table to Json String
;;;

(define (table->json-string t)

  (define (parse-value v)
    (cond
     ((string? v) (string-append "\"" v "\""))
     ((number? v) (number->string v))
     ((boolean? v) (if #t "true" "false"))
     ((list? v) (string-append "[" (let loop ((v v))
				     (if (= (length v) 1) (parse-value (car v))
					 (string-append (parse-value (car v)) ", " (loop (cdr v))))) "]"))
     ((table? v) (table->json-string v))
     (#t (raise 'value-not-encodable))))
  (string-append "{"
		 (let loop ((l (table->list t)))
		   (let* ((entry (car l))
			  (key (car entry))
			  (value (parse-value (cdr entry)))
			  (key (if (string? key) (string-append "\"" key "\": ") (raise 'key-not-string))))
		     (if (= (length l) 1) (string-append key value "}")
			 (string-append key value ", " (loop (cdr l))))))))


