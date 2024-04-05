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
;; Date: 2024-03-17
;; email: ivan@axoinvent.com
;; Project: List Utility 
;;

;;;
;;;; Flatten
;;;

(define (flatten l)
  (let loop ((l l) (r '()))
    (cond
     ((null? l) r)
     ((list? (car l)) (append (loop (car l) r) (loop (cdr l) r)))
     (#t (cons (car l) (loop (cdr l) r))))))

;;;
;;;; Pairize
;;;

(define (pairize l)
      (if (null? l) '()
       (cons (cons (car l) (cadr l)) (pairize (list-tail l 2)))))

;;;
;;;; Collect less than
;;;

(define (collect p v l)
  (cond
   ((null? l) '())
   ((p v (car l)) (cons (car l) (collect p v (cdr l))))
   (#t (collect p v (cdr l)))))

;;;
;;;; Sort List
;;;

(define (sort l)
  (if (null? l) '()
      (let ((v (list-ref l (random-integer (length l)))))
	(append (sort (collect > v l)) (list v) (sort (collect < v l))))))

