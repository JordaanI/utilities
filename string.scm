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
;; Date: 2024-03-16
;; email: ivan@axoinvent.com
;; Project: String Utilities
;;

;;;
;;;;String Splitter
;;;

(define (split-string str d)
  (let loop ((s 0) (e 0))
    (cond
     ((> e (string-length str)) '())
     ((or (= e (string-length str)) (char=? (string-ref str e) d)) (cons (substring str s e) (loop (+ e 1) (+ e 1))))
     (#t (loop s (+ e 1))))))

;;;
;;;; String Contain
;;;

(define (string-contain? s c)
  (member c (string->list s)))

;;;
;;;; substring
;;;

(define (substring s b e)
  (list->string
   (let loop ((cl (list-tail (string->list s) b))
	      (d (- e b)))
     (if (zero? d) '()
	 (cons (car cl) (loop (cdr cl) (- d 1)))))))

;;;
;;;; Strip Space
;;;

(define (strip-char s c)
  (list->string
   (let loop ((cl (string->list s)))
     (cond
      ((null? cl) '())
      ((char=? (car cl) c) (loop (cdr cl) c))
      (#t (cons (car cl) (loop (cdr cl) c)))))))
  
