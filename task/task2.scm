; Task 2 (Family Tree)
(define alphabet
  (read (open-input-file "./alphabet")))
(define tokugawa
  (read (open-input-file "./tokugawa")))

; Ext 1
(define (get-depth tree depth)
  (cond
    ((null? tree) '()) 
    ((= depth 0) 
     (if (pair? tree) 
         (if (list? (car tree))
            '()
            (list (car tree)))
         '()))
    ((list? tree)
     (apply append (map (lambda (sub-tree) (get-depth sub-tree (- depth 1)))
                        tree)))
    (else '())))

; Ext 2
(define (search tree name depth)
    (cond
      ((null? tree) 0)
      ((equal? tree name) (- depth 1))
      ((list? tree)
       (let ((result (map (lambda (sub-tree) (search sub-tree name (+ depth 1)))
                    tree)))
         (let loop ((lst result))
           (cond
             ((null? lst) 0)
             ((not (equal? 0 (car lst))) (car lst))
             (else (loop (cdr lst)))))))
      (else 0)))
       
(define (get-cousin tree name)
  (let ((depth (search tree name 0)))
    (define (get-cousin-helper tree current-depth)
      (cond
        ((null? tree) '())
        ((= current-depth depth)
         (if (pair? tree)
            (if (list? (car tree))
               '()
               (list (car tree)))
            '()))
        ((list? tree)
         (apply append (map (lambda (sub-tree) (get-cousin-helper sub-tree (+ current-depth 1)))
                           tree)))
        (else '())))
    (get-cousin-helper tree 0)))

; Ext 3
(define (get-path tree name)
  (define (helper tree path)
    (cond
      ((null? tree) #f)
      ((equal? tree name) (reverse path))
      ((list? tree)
       (let ((results (map (lambda (sub-tree) (helper sub-tree (cons (car tree) path))) tree)))
         (let loop ((lst results))
           (cond
             ((null? lst) #f)
             ((not (equal? #f (car lst))) (car lst))
             (else (loop (cdr lst)))))))
      (else #f)))
  (helper tree '()))