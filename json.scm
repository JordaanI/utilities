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
      (let ((key-val-pairs (split-string (strip-char (substring js 1 (- (string-length js) 1)) #\space) #\,))
	    (t (make-table)))
	(for-each (lambda (pair)
		    (table-set! t (car pair) (cdr pair)))
		  (split-string key-val-pairs #\:))
	t)
      #f))

;;;
;;;; Tests
;;;

(define t (list->table `(("1" . 2) ("2" . (1 (1 2) 2)) ("3" . "three"))))
(table-set! t "4" (table-copy t))
(define js (table->json-string t))
(println "json-string -> " js)
(println "current encoding -> " (table->list (json-string->table js)))
