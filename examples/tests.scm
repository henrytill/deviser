;;; Some code to test...

(define succ
  (lambda (n)
    (lambda (f)    ; we only support old-school lambda definitions
      (lambda (x)
        (f ((n f) x))))))

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define add1
  (lambda (x)
    (+ x 1)))

(define unchurch
  (lambda (n)
    ((n add1) 0)))

(define church
  (lambda (n)
    (let ((loop (lambda (n acc)
                  (if (eq? 0 n)
                      acc
                      (loop (- n 1) (succ acc))))))
      (loop n zero))))

(eq? 0 (unchurch (church 0)))

(eq? 42 (unchurch (church 42)))

(define three (succ (succ (succ zero))))

(eq? three 3)
;; #f

(eq? three (succ (succ (succ zero))))
;; #t

(eq? 3 (eval '(unchurch three)))
;; #t

(define nil '())

(define nil?
  (lambda (x)
    (eq? x nil)))

(define count
  ;; not stack-safe
  (lambda (x)
    (if (nil? x)
        zero
        (succ (count (cdr x))))))

(define xs '(5 6 7 8 9))

(eq? 5 (unchurch (count xs)))
;; #t

(car xs)
;; 5

(cdr xs)
;; '(6 7 8 9)

(define id
  (lambda (x)
    x))

((lambda x (car x)) '(1 2 3 4))
;; '(1 2 3 4)

(eq? 4 (unchurch ((lambda x (count x)) 1 2 3 4)))
;; #t

(define block
  (cons 'begin
        '((define quux zero)
          (succ (succ (succ quux))))))

(eq? 3 (unchurch (eval block)))
;; #t

;;; Products
(define pair
  (lambda (x y)
    (lambda (z)
      (z x y))))

(define first
  (lambda (p)
    (p (lambda (x y) x))))

(define second
  (lambda (p)
    (p (lambda (x y) y))))

(define p (pair 'left 'right))

(eq? 'left (first p))

(eq? 'right (second p))

;;; Sums
(define inl
  (lambda (a)
    (lambda (f g)
      (f a))))

(define inr
  (lambda (a)
    (lambda (f g)
      (g a))))

(define either
  (lambda (x y z)
    (z x y)))

(eq? 42 (either (lambda (x) (+ x 42))
                (lambda (x) (eq? "right" x))
                (inl 0)))

(eq? #t (either (lambda (x) (+ x 42))
                (lambda (x) (eq? "right" x))
                (inr "right")))
