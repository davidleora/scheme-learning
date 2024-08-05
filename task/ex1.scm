; Task 1 (Tree)
(define TREE '(1 (2 (3 4)) 6 (7 8 9)))
(define TREE1 '(1 (2) (3 (4) (5))))
(define TREE2 '(8 (3 (1 6 (4 7))) 10 (14 13)))
(define TREE3 '(5 (10 (20 (40) (50 (80) (90 (110)))) (30 (60 (100)) (70))) (15 (25 (35 (55)) (45 (65 (105) (115))))) (12 (22 (32 (42) (52))) (17 (27 (37) (47 (67)))) (14 (24 (34))))))

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
                       
