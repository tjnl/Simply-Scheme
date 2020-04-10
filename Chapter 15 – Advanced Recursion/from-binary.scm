(define (from-binary n)
  (fb-helper n 0 0))

(define (fb-helper n pos result)
  (if (empty? n)
      result
      (fb-helper (bl n) (+ 1 pos) (+ result (* (last n) (expt 2 pos))))))

(define (fb n)
  (if (empty? n)
      0
      (+ (* (fb (bl n)) 2) (last n))))