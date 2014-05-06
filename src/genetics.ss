(set! population '())

(define (run lst gen-one-size)
  (set! target lst)
  (first-gen (length lst) gen-one-size)
  (display population))

(define (first-gen chrom-len i)
  (if (> i 0)
      (begin
        (let ((x (gen-chrom '() chrom-len)))
          (set! population (append population (list x))))
        (first-gen chrom-len (- i 1)))
      population))

(define (gen-chrom lst chrom-len)
  (if (< (length lst) chrom-len)
      (gen-chrom (append lst (list (random 2))) chrom-len)
      lst))

(run '(1 1 1 1) 10)