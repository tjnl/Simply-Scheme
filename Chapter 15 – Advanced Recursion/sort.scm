(define (srt sent)
  (if (empty? sent)
      '()
      (se (earliest-word sent)
          (srt (remove-once (earliest-word sent) sent)))))

(define (earliest-word sent)
  (earliest-helper (first sent) (bf sent)))

(define (earliest-helper so-far rest)
  (cond ((empty? rest) so-far)
	((before? so-far (first rest))
	 (earliest-helper so-far (bf rest)))
	(else (earliest-helper (first rest) (bf rest)))))

(define (remove-once wd sent)
  (cond ((empty? sent) '())
	((equal? wd (first sent)) (bf sent))
	(else (se (first sent) (remove-once wd (bf sent))))))
