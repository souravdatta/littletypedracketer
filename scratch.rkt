#lang typed/racket

(require typed/rackunit)

(: rember* (All (a) (-> a (Listof Any) (Listof Any))))
(define (rember* s l)
  (if (empty? l)
      l
      (let ([f (car l)])
        (if (list? f)
            (cons (rember* s f)
                  (rember* s (cdr l)))
            (if (equal? f s)
                (rember* s (cdr l))
                (cons f (rember* s (cdr l))))))))


(check-equal? (rember* 'cup
                       '((coffee) cup ((tea) cup)
                                  (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))


(: insertR* (All (a) (-> a a (Listof Any) (Listof Any))))
(define (insertR* new old l)
  (if (empty? l)
      l
      (let ([f (car l)])
        (if (list? f)
            (cons (insertR* new old f)
                  (insertR* new old (cdr l)))
            (if (equal? f old)
                (cons f (cons new (insertR* new old (cdr l))))
                (cons f (insertR* new old (cdr l))))))))


(check-equal? (insertR* 'roast
                        'chuck
                        '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))


(: subst* (All (a) (-> a a (Listof Any) (Listof Any))))
(define (subst* new old l)
  (if (empty? l)
      l
      (let ([f (car l)])
        (if (list? f)
            (cons (subst* new old f)
                  (subst* new old (cdr l)))
            (if (equal? f old)
                (cons new (subst* new old (cdr l)))
                (cons f (subst* new old (cdr l))))))))

(check-equal? (subst* 'orange
                      'banana
                      '((banana)
                        (split ((((banana ice)))
                                (cream (banana))
                                sherbet))
                        (banana)
                        (bread)
                        (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                         (cream (orange))
                         sherbet))
                (orange)
                (bread)
                (orange brandy)))
