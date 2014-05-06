(define (population '()))
(define fitness-array '())

;; Called at program start, takes first generation and begins evolution
(define (run lst gen-one-size)
  (set! target lst)
  (set! target-fitness (calculate-fitness lst 0))
  (set! population-size gen-one-size)
  (first-gen (length lst) population-size)
  (evolve #t))

;; Creates the first generation of chromosome strings
(define (first-gen chrom-len i)
  (if (> i 0)
      (begin
        (let ((x (gen-chrom '() chrom-len)))
          (set! population (append population (list x))))
        (first-gen chrom-len (- i 1)))
      population))

;; Creates a single, random chromosome string
(define (gen-chrom lst chrom-len)
  (if (< (length lst) chrom-len)
      (gen-chrom (append lst (list (random 2))) chrom-len)
      lst))

;; Evolves the population by one generation until desired outcome achieved
(define (evolve continue)
  (if (continue)
      (begin
        (update-generation population)
        (population-fitness population)
        (evolve (check-fitness fitness-array)))
      (begin
        (display "Target chromosome achieved.")
        (newline)
        (display "Final population:")
        (newline)
        (display population))))

;; Checks population fitnesses to desired outcome fitness
(define (check-fitness lst)
  (if (not (null? lst))
      (if (= (car (car lst)) target-fitness)
          (if (match-chrom (cadr (car lst)) target)
              #f
              (check-fitness (cdr lst)))
          #t)
      #t))

;; Recurses through two chromosomes; return true if a match, otherwise false
(define (match-chrom lst1 lst2)
  (if (not (null? lst1))
      (if (= (car lst1) (car lst2))
          (match-chrom (cdr lst1) (cdr lst2))
          #f)
      #t))

;; Updates fitness-array for current population
(define (population-fitness population)
  (set! fitness-array
        (map
         (lambda (x)
           (list (calculate-fitness x 0) x))
         population)))

;; Calculates fitness of a chromosome
(define (calculate-fitness child fitness)
  (if (null? child)
      fitness
      (if (eqv? 1 (car child))
          (calculate-fitness (cdr child) (+ fitness 1))
          (calculate-fitness (cdr child) fitness))))

(run '(1 1 1 1) 10)