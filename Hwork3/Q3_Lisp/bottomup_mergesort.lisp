#|
Kevin Gutierrez
CSCI 330 Programming Language Concepts
Recursive bottom-up mergesort

Description: Bottom up mergesort - lisp recursive implementation
|#

; To run: sbcl --load ./bottomup_mergesort.lisp

#| This function partitions the input list into pairs recursively, such that given an input list
      (1 2 3 4 5 6), the output is ((1 2) (3 4) (5 6))
|#
(defun partition_pairs (input_list)
  (cond(
      ;If input list is empty return null
      (endp input_list) nil)
    (
      ;If the list has only one element then
      (endp (cdr input_list)) 
      (list (list (car input_list))) ;Return a list containing a list with only the single elemnt
    )
    ;recursive case 
    (t ;t - else
      (let 
        (
          ;these are the pairs
          (first (car input_list)) ;first temp var := first item of the input list
          (second (cadr input_list)) ;second temp var := second item of the input list
        )
        (
          cons
          (
            ;if first is less than second then sort the _pair_
            if (< first second) 
              (list first second) 
              (list second first)
          )
          ;Do this recursively with the _elements after the first two_
          (partition_pairs (cddr input_list))
        )
      )
    )
  )
)

#| This function merges two lists, such that given two lists a = '(1 2 3 4 5) and 
    b = (6 7 8 9), the output is (1 2 3 4 5 6 7 8 9)
 |#
(defun merge_lists (first_list second_list)
  (cond
    ;base case - if both lists are empty, return null
    ((and
        (endp first_list )
        (endp second_list)
      ) nil 
    )
    ;if first list is empty, take first element of second_list and recursively call merge_lists with an empty first list and the rest of second_list
    ((endp first_list) 
        (cons 
          (CAR second_list)
          (merge_lists nil 
              (CDR second_list)
          )
        )
    )
    ;if second list is empty then:
    ((endp second_list) 
        ; take first element of first_list and recursively call itself with rest of first_list and empty second_list to build the merged list 
            ;by adding elements from first_list
        (cons 
            (CAR first_list) 
            (merge_lists 
                (CDR first_list) 
            NIL)
        )
    )
    ;if first element of first list is smaller than the first element of the second list then:
    ((< 
        (CAR first_list) 
        (CAR second_list)
    )   ; take first element of the first list and recursively call itself with the rest of first_list and second_list
        (cons 
            (CAR first_list) 
            (merge_lists 
                (CDR first_list) second_list  )
        )
    )
    ;if first element of first list is greater or equal to first element of first list then:
    ((>= 
        (CAR first_list) 
        (CAR second_list)
      )   ; take first element of second_list and recursively call itself with first_list and the rest of second_list to ensure that 
            ; <= element is added to merged list _first_
        (cons 
            (CAR second_list) 
            (merge_lists first_list 
                (CDR second_list)) 
        )
    )
  )
)

#| This function merges adjacent lists, such that given ((1 2)(3 4 5 6)(8 9)), the output is
    ((1 2 3 4 5 6) (8 9)). This is done recursively. |#
(defun merge_adjacent (input_list)
  (cond
    ; if input list is null return null
    ((endp input_list) nil)
    ; if input tail of input list is null, return first element of input list wrapped in a list
    ((endp (cdr input_list)) (list (car input_list)))
    ; recursive case - else define two variables, first and second, equal to the first and second elements of the input list
    (t (let ((first (car input_list))
             (second (cadr input_list)))
        ; then construct a new list with the first and second as params
        (cons
                        ;while constructing the combined adjacent lists, need to check if 
          (merge_lists (if (listp first) first (list first)) 
                       (if (listp second) second (list second)))
          (merge_adjacent (cddr input_list))))))) ;Recursively call itself with the input list, but without the first two elements

#| Recursively merges adjacent lists from the bottom up until there is only list remaining (which is sorted) |#
(defun bottom_up_merge (input_list)
  ;if the list contains only one list, return the list after the first element - this is the fully merged and sorted list
  (if (endp (cdr input_list))
    (car input_list)
    (bottom_up_merge (merge_adjacent input_list)))) ;Call itself recursively when the list contains more than one item

#| |#
(defun mergesort_bottomup (input_list)
  ;First convert the list to a list of sorted pairs
  (let ((sorted_pairs (partition_pairs input_list)))
    ;then, with the sorted pairs, start merging them 
    (bottom_up_merge sorted_pairs)))

