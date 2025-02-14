#|
Kevin Gutierrez
CSCI 330 Programming Language Concepts
Lisp insertion sort

|#

; To run: sbcl --load ./insertionsort.lisp


(defun insert (element sorted_list)
  (cond
    ; If the sorted list is empty return a list with the element
    ((endp sorted_list) (list element)) 
    ;If the element is less than or equal to the first element, insert it at the front.
    ((<= element (car sorted_list)) (cons element sorted_list)) 
    ;else insert the element into the sorted list
    (t (cons (car sorted_list) (insert element (cdr sorted_list)))))) ; Otherwise, insert it into the rest of the list.

(defun insertionsort (input_list)
  (cond
    ;If the list is empty return null
    ((endp input_list) nil)
    ;else inesrt the first element into the sorted list 
    (t (insert (car input_list) (insertionsort (cdr input_list)))))) 
