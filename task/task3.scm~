; Task 3
; Part 1. Differentiation
(define (diff expr)
  (cond
    ((number? expr) 0)
    ((eqv? expr 'x) 1)
    ((and (list? expr) (eqv? (car expr) '+)) (cons '+ (map diff (cdr expr))))
    ((and (list? expr) (eqv? (car expr) '-)) (cons '- (map diff (cdr expr))))
    ((and (list? expr) (eqv? (car expr) '*))
     (let ((u (cadr expr)) (v (caddr expr))) `(+ (* ,u ,(diff v)) (* ,(diff u) ,v))))
    ((and (list? expr) (eqv? (car expr) '**))
     (let ((u (cadr expr)) (n (caddr expr))) `(* ,n (* ,(diff u) (** ,u ,(- n 1))))))
))

; Part 2. Tangent
; Function to evaluate f(x)
(define ** expt)

(define tangent
  (lambda (f a)
    (let ((fa ((eval `(lambda (x) ,f) (interaction-environment)) a)))
      (let ((fdx (diff f)))
        (let ((slope ((eval `(lambda (x) ,fdx) (interaction-environment)) a)))
          `(+ (* ,slope x) ,(- fa (* slope a))))))))

; Part 3. Partial differentiation
(define diff2
  (lambda(expr1 expr2)
  (cond
    ((number? expr1) 0)
    ((symbol? expr1) (if (eqv? expr1 expr2) 1 0))
    ((and (list? expr1) (eqv? (car expr1) '+))
     (cons '+ (map (lambda (e) (diff2 e expr2)) (cdr expr1))))
    ((and (list? expr1) (eqv? (car expr1) '-))
     (cons '- (map (lambda (e) (diff2 e expr2)) (cdr expr1))))
    ((and (list? expr1) (eqv? (car expr1) '*))
     (let ((u (cadr expr1)) (v (caddr expr1))) `(+ (* ,u ,(diff2 v expr2)) (* ,(diff2 u expr2) ,v))))
    ((and (list? expr1) (eqv? (car expr1) '**))
     (let ((u (cadr expr1)) (n (caddr expr1)))
       (if (equal? (diff2 u expr2) 0)
           0
           `(* ,n (* ,(diff2 u expr2) (** ,u ,(if (number? n) (- n 1) `(0 ,n 1))))))))
    )))

#|
Redundancy Example
> (diff2 '(+ (* 3 (** x 2)) (* 2 y) (* 5 z)) 'x)
(+ (+ (* 3 (* 2 (* 1 (** x 1)))) (* 0 (** x 2))) (+ (* 2 0) (* 0 y)) (+ (* 5 0) (* 0 z)))
|#

; Part 4. Simple
(define remove-zeros
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((equal? (car lst) 0) (remove-zeros (cdr lst)))
      (else (cons (car lst) (remove-zeros (cdr lst))))
      )
    )
  )

(define simple+
  (lambda (lst)
    ;; TODO: Implement addition simplification logic
    (let ((non-zero-list (remove-zeros (cdr lst))))
      (cond
        ;; Case 1: If non-zero-list is empty
        ((null? non-zero-list) 0)
        
        ;; Case 2: If non-zero-list has one element
        ((null? (cdr non-zero-list)) (car non-zero-list))
        
        ;; Case 3: If non-zero-list has multiple elements
        (else (cons '+ non-zero-list))))
    ))


(define simple-
  (lambda (lst)
    ;; TODO: Implement subtraction simplification logic
    (let ((non-zero-list (cdr lst)))
      (cond
        ((null? non-zero-list) (car lst))
        (else (cons '- non-zero-list))))
    ))

#|
(define simple*
  (lambda (lst)
    ;; TODO: Implement multiplication simplification logic
    ))

(define simple**
  (lambda (lst)
    ;; TODO: Implement exponentiation simplification logic
    ))
|#