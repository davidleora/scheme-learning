#|
integersfrom: 無限リストを生成する関数である。
引数 n から始まる無限リストを生成する。
リストは遅延評価を用いて生成されるため、必要なときに評価される。
|#
(define (integersfrom n)
  (cons n (lambda () (integersfrom (+ n 1))))
  )

#|
filterdiv: 指定した num で割り切れる数を除外する関数である。
stream のリストを処理し、num で割り切れる要素を取り除いた新しいリストを返す。
|#
(define (filterdiv num stream)
  (cond
    ((null? stream) '())
    ((zero? (modulo (car stream) num))
     ((lambda () (filterdiv num ((cdr stream))))))
    (else
     (cons (car stream)
           (lambda () (filterdiv num ((cdr stream))))))
    ))

#|
sieve: エラトステネスの篩を用いて素数の無限リストを生成する関数である。
stream の最初の要素を素数とみなし、それで割り切れる要素を除去する。
その後、再帰的に処理を繰り返して素数リストを生成する。
|#
(define (sieve stream)
  (cons (car stream)
        (lambda () (sieve (filterdiv (car stream) ((cdr stream)))))
        )
  )

#|
primelist: 2 から始まる無限の素数リストを生成する関数である。
sieve を用いて、エラトステネスの篩を適用し、素数のみを含むリストを作成する。
|#
(define (primelist)
  (sieve (integersfrom 2))
  )

#|
head: 無限リストの先頭 n 要素を取り出す関数である。
無限リスト stream の先頭から n 個の要素をリストとして返す。
|#
(define (head n stream)
  (if (= n 0) '()
      (cons (car stream)
            (head (- n 1) ((cdr stream)))))
  )