; Task 2 (Family Tree)
(define alphabet
  (read (open-input-file "./alphabet")))
(define tokugawa
  (read (open-input-file "./tokugawa")))

; Ext 1
(define (get-depth tree depth)
  (cond
    ((null? tree) '())  ; Handle an empty tree
    ((= depth 0)        ; We're at the target depth
     (if (pair? tree)   ; If tree is a pair (i.e., non-leaf node)
         (if (list? (car tree))  ; If car is a list, skip it at depth 1
             '()
             (list (car tree)))  ; Collect non-list car elements
         '()))
    ((list? tree)       ; If tree is a list and not at depth 1
     (apply append (map (lambda (sub-tree)  ; Apply to each subtree
                          (get-depth sub-tree (- depth 1)))
                        tree)))
    (else '())))  ; Return an empty list in other cases




