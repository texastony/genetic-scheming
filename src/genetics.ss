(set! population '())
(set! gen-one-size 10)

(define (run lst)
  (set! target lst)
  (gen-one (length lst) gen-one-size))

(define (gen-one chrom-len i)
  (if (> i 0)
      (begin
        (let (chrom (gen-chrom '() chrom-len))
          (append population chrom)))
      ()))

;; so this'll recurse for the length of the chromosome, appending randomly generated 1s and 0s
(define (gen-chrom lst chrom-len)
  (if (< (length lst) chrom-len)
      (append lst (random 1))
      