(define (square lst)
  (map (lambda (x) (* x x)) lst)
)

(define (sum lst)
  (apply + lst)
)

(define (rms lst)
  (sqrt(/ (sum (square lst)) (length lst)))
)