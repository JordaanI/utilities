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
;;;; Is Json string
;;;

(define (is-json-string? string)
  (let ((cl (string->list string)))
    (and (char=? (car cl) #\{) (char=? (car (reverse cl)) #\}))))

;;;
;;;; json-string->table
;;;

(define (json-string->table js)

  (define (parse-value val)
    #t)
  
  (if (is-json-string? js)
      (let ((scs (strip-char (substring js 1 (- (string-length js) 1)) #\space))
	    (t (make-table)))
	(let loop ((cl (string->list scs)) (in-array? #f) (s-index 0) (e-index 0))
	  (cond
	   ((null? cl)
	    (let ((key-val (split-string (substring scs s-index e-index) #\:)))
	      (and (table-set! t) t (car key-val) (cdr key-val))))
	   ((and (not in-array?) (char=? (car cl) #\,))
	    (let ((key-val (split-string (substring scs s-index e-index) #\:)) (new-index (+ e-index 1)))
	      (table-set! t (car key-val) (cdr key-val))
	      (loop (cdr cl) in-array? new-index new-index)))
	   ((or (char=? (car cl) #\]) (char=? (car cl) #\[)) (loop (cdr cl) (not in-array?) s-index (+ e-index 1)))
	   (#t (loop (cdr cl) in-array? s-index (+ e-index 1))))))
      #f))

;;;
;;;; Tests
;;;

(define t (list->table `(("1" . 2) ("2" . (1 (1 2) 2)) ("3" . "three"))))
(table-set! t "4" (table-copy t))
(define js (table->json-string t))
(println "json-string -> " js)
;(println "current encoding -> " (table->list (json-string->table js)))
