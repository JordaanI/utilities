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
;; Project: Utilities main file 
;;

(define-library (github.com/JordaanI/utilities utilitites)

  (export
					; String Utilities
   split-string
   string-contain
   strip-char
   substring

					; List Utilities
   flatten

					; Json Utilities
   table->json-string
   json-string->table

					;testing Vars
   t
   js)
  
  (import
   ; String Utilities
   (.. string)
   ; List Utilities
   (.. list)
   ; Json Utilities
   (.. json)))
