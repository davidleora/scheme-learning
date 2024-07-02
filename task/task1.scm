; Task 1 (Tree)
(define TREE '(1 (2 (3 4)) 6 (7 8 9)) )
(define TREE2 '(1 (2) (3 (4) (5))))

; Ext 1
(define tree-member?
  (lambda (tree val)
    (cond
      ((null? tree) #f)
      ((pair? tree)
       (or (tree-member? (car tree) val)
          (tree-member? (cdr tree) val)))
      (else (equal? tree val))
      )))

; Ext 2
(define map-tree
  (lambda (fn tree)
    (cond ((null? tree) '())
         ((pair? tree)
          (cons (map-tree fn (car tree))
               (map-tree fn (cdr tree))))
         (else (fn tree)))))

; Ext 3
(define flatten
  (lambda(tree)
    (cond
      ((null? tree) '())
      ((pair? tree)
       (append (flatten (car tree))
              (flatten (cdr tree))))
      (else (list tree))
      )))
                       