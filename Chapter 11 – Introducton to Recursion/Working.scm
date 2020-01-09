(define (downup wd)
  (if (= 1 (count wd))
      (se wd)
      (se wd (downup (bl wd)) wd)))

(define (pigl wd)
  (if (member? (first wd) 'aeiouy)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (explode wd) ;; doesn't handle empty words
  (if (= 1 (count wd))
      (se wd)
      (se ((first wd) (explode (bf wd))))))

(define (exp wd) ;; amended
  (if (empty? wd)
      '()
      (se (first wd) (exp (bf wd)))))

(define (letter-pairs wd) ;; doesn't handle words of less than 2 letters
  (if (= 2 (count wd))
      (se wd)
      (se (word (first wd) (first (bf wd))) (letter-pairs (bf wd)))))

(define (lp wd) ;; amended
    (if (<= (count wd) 1)
        '()
        (se (word (first wd) (first (bf wd))) (lp (bf wd)))))