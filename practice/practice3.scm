(define times
  (lambda (n ls)
    (map (lambda (x) (* x n)) ls)
  )
)

; recursive method
(define timess
  (lambda (n ls)
    (if (null? ls) '()
       (cons (* n (car ls)) (timess n (cdr ls))))
    )
  )