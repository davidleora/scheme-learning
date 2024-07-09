; Task 3
; Part 1. Differentiation
(define (diff expr)
  (cond
    ((number? expr) 0)
    ((eqv? expr 'x) 1)
    ((and (list? expr) (eqv? (car expr) '+)) (cons '+ (map diff (cdr expr))))
    ((and (list? expr) (eqv? (car expr) '-)) (cons '- (map diff (cdr expr))))
    ((and (list? expr) (eqv? (car expr) '*))
     (let ((u (cadr expr)) (v (caddr expr))) `(+ (*, u, (diff v)) (*, (diff u), v))))
    ((and (list? expr) (eqv? (car expr) '**))
     (let ((u (cadr expr)) (n (caddr expr))) `(*, n (*, (diff u) (**, u, (- n 1))))))
))

(display (diff 'x))
(newline)
(display (diff '(+ x 5)))
(newline)
(display (diff '(+ (** x 3) (* 2 x) 4)))
(newline)
(display (diff '(** (* x 3) 2)))
(newline)
(display (diff '(* (+ x 2) (- (** x 2) x))))
(newline)