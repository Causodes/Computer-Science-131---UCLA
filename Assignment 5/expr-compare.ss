#lang racket

(define lambda_2 (string->symbol "λ"))

(define (bind x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (index lst i x) (if (equal? x (car lst)) i (index (cdr lst) (+ i 1) x)))

(define (lambda_resolver x y) (if (equal? x y) x lambda_2))

(define (binder x y a b c) (cond
  [(and (equal? x '()) (equal? y '())) (list a b c)]
  [(equal? (car x) (car y)) (binder (cdr x) (cdr y) a b c)]
  [else (binder (cdr x) (cdr y) (cons (bind (car x) (car y)) a) (cons (car x) b) (cons (car y) c))]))
  
(define (binder_2 x y a b) (cond
  [(equal? x '()) '()]
  [(list? (car x)) (cons (binder_2 (car x) y a b) (binder_2 (cdr x) y a b))]
  [(member (car x) a) 
							(cons (let ((i (index-of a (car x)))) (list-ref b i)) (binder_2 (cdr x) y a b))]
  [else (cons (car x) (binder_2 (cdr x) y a b))]))

(define (bind_x_var x y a b) (cond
  [(equal? x '()) '()]
  [(or (and (list? (car x)) (not (list? (car y)))) (and (not (list? (car x))) (list? (car y)))
	(and (list? (car x)) (list? (car y)) (not (equal? (length (car x)) (length (car y))))))
	(cons (constant_handler (car x) (car y)) (bind_x_var (cdr x) (cdr y) a b))]
  [(and (list? (car x)) (or (equal? (car (car x)) 'lambda) (equal? (car (car y)) 'lambda)
                            (equal? (car (car x)) lambda_2) (equal? (car (car y)) lambda_2)))
							(cons (expr-compare (car x) (car y)) (bind_x_var (cdr x) (cdr y) a b))]
  [(list? (car x)) (cons (bind_x_var (car x) (car y) a b) (bind_x_var (cdr x) (cdr y) a b))]
  [(member (car x) b) (cons (let ((i (index b 0 (car x)))) (list-ref a i)) (bind_x_var (cdr x) (cdr y) a b))]
  [else (cons (car x) (bind_x_var (cdr x) (cdr y) a b))]))

(define (bind_y_var y x a b) (cond
  [(equal? y '()) '()]
  [(or (and (list? (car y)) (not (list? (car x)))) (and (not (list? (car y))) (list? (car x)))
	(and (list? (car y)) (list? (car x)) (not (equal? (length (car y)) (length (car x))))))
	(cons (constant_handler (car x) (car y)) (bind_y_var (cdr y) (cdr x) a b))]
  [(and (list? (car y)) (or (equal? (car (car y)) 'lambda) (equal? (car (car x)) 'lambda)
                            (equal? (car (car y)) lambda_2) (equal? (car (car x)) lambda_2)))
							(cons (expr-compare (car x) (car y)) (bind_y_var (cdr y) (cdr x) a b))]
  [(list? (car y)) (cons (bind_y_var (car y) (car x) a b) (bind_y_var (cdr y) (cdr x) a b))]
  [(member (car y) b) (cons (let ((i (index b 0 (car y)))) (list-ref a i)) (bind_y_var (cdr y) (cdr x) a b))]
  [else (cons (car y) (bind_y_var (cdr y) (cdr x) a b))]))

(define (lambda_handler x y)
  (let ((x1 (list-ref x 1)) (y1 (list-ref y 1)))
    (let ((b 
	(if(and (list? x1) (list? y1) (equal? (length x1) (length y1))) (binder x1 y1 '() '() '())
      (if(and (not (list? x1)) (not (list? y1))) (binder (list x1) (list y1) '() '() '())
		#f))))
        (if b
          (cons (lambda_resolver (car x) (car y))
            (expr-compare
              (bind_x_var (cdr x) (cdr y) (car b) (list-ref b 1))
              (bind_y_var (cdr y) (cdr x) (car b) (list-ref b 2))))
	  (constant_handler x y)))))

(define (predicate_handler x y) (cond
  [(equal? (car x) 'quote) (constant_handler x y)]
  [(equal? (car x) 'lambda) (lambda_handler x y)]
  [(equal? (car x) lambda_2) (lambda_handler x y)]
  [else (list_handler x y)]))

(define (predicate_handler_2 x y) (cond
  [(or (equal? (car x) 'quote) (equal? (car y) 'quote) (equal? (car x) 'if) (equal? (car y) 'if)) (constant_handler x y)]
  [(or (and (equal? (car x) 'lambda) (equal? (car y) lambda_2))
							(and (equal? (car y) 'lambda) (equal? (car x) lambda_2))) (lambda_handler x y)]
  [else (list_handler x y)])) 

(define (constant_handler x y) (cond
  [(equal? x y) x]
  [(and (equal? x #t) (equal? y #f)) '%]
  [(and (equal? x #f) (equal? y #t)) '(not %)]
  [else (list 'if '% x y)]))

(define (list_handler x y)
  (if (or (equal? x '()) (equal? y '())) '()
    (cons (expr-compare (car x) (car y)) (list_handler (cdr x) (cdr y)))))

(define (expr-compare x y)
  (if (and (list? x) (list? y))
    (if (equal? (length x) (length y))
      (if (equal? (car x) (car y))
        (predicate_handler x y)
        (predicate_handler_2 x y))
      (constant_handler x y))
    (constant_handler x y)))
	
(define (test-expr-compare x y) 
	(and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))
	
(define test-expr-x '(list "tism" (lambda (a b) (if (equal? (a b)) 1 2))))
(define test-expr-y '(list "bism" (λ (a c) (if (eqv? (a c)) 1 2))))

(test-expr-compare test-expr-x test-expr-y)