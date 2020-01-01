;; see Simply Scheme, pp.141–144.

(define (card-val card)
  (let ((card-rank (bf card)))
    (cond ((number? card-rank) 0)
          ((equal? card-rank 'j) 1)
          ((equal? card-rank 'q) 2)
          ((equal? card-rank 'k) 3)
          ((equal? card-rank 'a) 4))))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

(define (count-suit suit hand)
  (count (keep (lambda (card) (equal? suit (first card))) hand)))

(define (suit-counts hand)
  (every (lambda (suit) (count-suit suit hand)) '(s h c d)))

(define (suit-dist-points num)
  (cond ((> num 2) 0)
        ((= num 2) 1)
        ((= num 1) 2)
        (else 3)))

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand))))

(define (bridge-val hand)
  (+ (high-card-points hand) (hand-dist-points hand)))