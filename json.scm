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

;;;
;;;; Compare first and last string
;;;

(define (compare-head-tail s h t)
  (let ((cl (string->list s)))
    (and (char=? (car cl) h) (char=? (car (reverse cl)) t))))

;;;
;;;; Is Json string
;;;

(define (is-json-string? s)
  (compare-head-tail s #\{ #\}))

;;;
;;;; is Json List
;;;

(define (is-json-list? s)
  (compare-head-tail s #\[ #\]))

;;;
;;;; json-string->table
;;;

(define (json-string->table js)

  (define (parse-string s)
    (list->string
     (let loop ((cl (string->list s)))
       (cond
	((null? cl) '())
	((not (or (char=? (car cl) #\\) (char=? (car cl) #\"))) (cons (car cl) (loop (cdr cl))))
	(#t (loop (cdr cl)))))))
  
  (define (parse-string-number s)
    (let* ((c (parse-string s))
	   (number (string->number c)))
      (if number number c)))
  
  (define (parse-list s)
    (let ((sl (split-string (substring s 1 (- (string-length s) 1)) #\,)))
      (map parse-value sl)))
  
  (define (parse-value val)
    (cond
     ((is-json-string? val) (json-string->table val))
     ((is-json-list? val) (parse-list val))
     ((string=? val "true") #t)
     ((string=? val "false") #f)
     (#t (parse-string-number val))))

  (define (get-key-val scs s e)
    (split-string (substring scs s e) #\:))

  (define (set-in t key-val)
    (table-set! t (parse-string (car key-val)) (parse-value (cadr key-val))))
  
  (if (is-json-string? js)
      (let ((scs (strip-char (substring js 1 (- (string-length js) 1)) #\space))
	    (t (make-table)))
	(let loop ((cl (string->list scs)) (in-array? #f) (s-index 0) (e-index 0))
	  (cond
	   ((null? cl)
	    (let ((key-val (get-key-val scs s-index e-index)))
	      (set-in t key-val)
	      t))
	   ((and (not in-array?) (char=? (car cl) #\,))
	    (let ((key-val (get-key-val scs s-index e-index)) (new-index (+ e-index 1)))
	      (set-in t key-val)
	      (loop (cdr cl) in-array? new-index new-index)))
	   ((or (char=? (car cl) #\]) (char=? (car cl) #\[)) (loop (cdr cl) (not in-array?) s-index (+ e-index 1)))
	   (#t (loop (cdr cl) in-array? s-index (+ e-index 1))))))
      #f))
