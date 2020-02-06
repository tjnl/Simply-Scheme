(define (number-name n)
  (if (= n 0)
      'zero
      (large-number-helper (numbers-to-sentence n) large-numbers)))

(define large-numbers '(thousand million billion trillion quadrillion quintillion sextillion septillion octillion nonillion decillion))

(define (numbers-to-sentence n)
  (if (word? n)
      (numbers-to-sentence (se n))
      (let ((first-n (first n)))
        (if (< (count first-n) 3)
            n
            (numbers-to-sentence (se (bl (bl (bl first-n)))
                                    (word (last (bl (bl first-n)))
                                          (last (bl first-n))
                                          (last first-n))
                                    (bf n)))))))

(define (small-number-helper n)
  (cond ((and (= (count n) 3) ;; 100–999
              (> (first n) 0))        
         (se (small-number-helper (first n)) 'hundred (small-number-helper (bf n))))
        ((> n 19)             ;; 20–99
         (se (item (- (last (bl n)) 1) '(twenty thirty forty fifty sixty seventy eighty ninety)) (small-number-helper (last n))))
        ((> n 9)              ;; 10–19
         (item (+ (last n) 1) '(ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen)))
        ((> n 0)              ;; 1–9
         (item n '(one two three four five six seven eight nine)))
        (else '())))          ;; 0

(define (large-number-helper n large-numbers)
  (cond ((empty? n) '())
        ((= (count n) 1) (small-number-helper (first n)))
        (else (let ((y (last (bl n)))
                    (z (last n)))
                (cond ((and (= y 0) (= z 0)) (large-number-helper (bl n) (bf large-numbers))) ;; 000 000
                      ((and (> y 0) (= z 0)) (se (large-number-helper (bl n) (bf large-numbers)) (first large-numbers))) ;; 111 000
                      ((and (> y 0) (> z 0)) (se (large-number-helper (bl n) (bf large-numbers)) (first large-numbers) (small-number-helper z))) ;; 111 111
                      (else (se (large-number-helper (bl n) (bf large-numbers)) (small-number-helper z)))))))) ;; 000 111