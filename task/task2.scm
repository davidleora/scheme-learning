; Task 2 (Family Tree)
(define alphabet
  (read (open-input-file "./alphabet")))
(define tokugawa
  (read (open-input-file "./tokugawa")))

; Ext 1
(define get-depth
  (lambda (tree depth)
    (cond
      ((= depth 0) (list tree))
      ((pair? tree) (append (get-depth (car tree) (- depth 1))
                           (get-depth (cdr tree) depth)))
      (else '()))
    ))