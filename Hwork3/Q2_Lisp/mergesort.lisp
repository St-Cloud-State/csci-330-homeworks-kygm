#|
Kevin Gutierrez
CSCI 330 Programming Language Concepts
Recursive mergesort - lisp implementation

Description: This algorithm splits a list into sublists until each sublist has only one item, then the sublists are all
merged into a sorted list

Notes:
    endp construct returns true if list is empty, else false
    cond construct is a condition - similar to an if statement 

|#

; To run: sbcl --load ./mergesort.lisp


(defun recursive_partition (input_list)
    (if 
        ;Base case - Check if the list is empty, if it is, return a list wiht a null head and tail to mark termination, 
        ;we will check for this in calls to this function
        (endp input_list)
            '(nil nil)

        ;Recursive case
        (let 
            (
                (   ;Call iteself on the tail
                    partition (recursive_partition (cdr input_list))
                )
            )
            ( list ;The two subsequent sublists are joined

                ;first sublist constructed by adding the first element of the current list (car) to the second element
                ;of sublist from the recursive call (cadr) 
                (
                        ;first list         ;second list
                    cons (car input_list) (cadr partition)
                ) 
                (
                    car partition
                ) 
            )
        )
    )
)

(defun merge_lists (first_list second_list)
    (cond
        ;base case - if both lists are empty, return null
        ((and
            (endp first_list )
            (endp second_list)) nil 
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

(defun mergesort (input_list)
    (cond 
        ;base case
        ( 
            ;if input list is empty, return null
            (endp input_list) 
                nil
        )
        (
            ;if input_list has only one element, return input_list
            (endp (cdr input_list)) 
                input_list
        )
        ;recursive case
        (t
            ;sequentially bind the subsequent vars that rely on previous bindings - this is necessary because each binding depends on
                ; the binding that precedes it (recursive calls)
            (let* 
                (
                    ;split the input_list into two sublists
                    (partitioned_lists
                        (recursive_partition input_list )
                    )
                    ;left_partition is obtained from the head of partitioned_lists
                    (left_partition  
                        (car partitioned_lists)
                    )
                    ;right_partition is obtained from the tail of partitioned_lists
                    (right_partition 
                        (cadr partitioned_lists)
                    )
                    ;Obtain the sorted left list by recursively calling itself with the left_partition
                    (sorted_left 
                        (mergesort left_partition )
                    )
                    ;Obtain the sorted right list by recursively calling itself with the right_partition
                    (sorted_right 
                        (mergesort right_partition))
                )
                ;Finlly, when the final sorted lists are obtained, merge them
                (merge_lists sorted_left sorted_right)
            )
        )
    )
)
