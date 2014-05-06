(define population '())
(define fitness-array '())
(define counter 0)
(define updateFrequency 0) 
(define population-size 0)
(define stochastic-array '())
(define target-fitness 0)
(define stopper 1)

;; Called at program start, takes first generation and begins evolution
(define (run target populationsize x)
  (set! target-fitness target)
  (set! population-size populationsize)
  (set! updateFrequency x)
  (first-gen target population-size)
  (update-generation population)
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
;;  (display population)
;;  (newline)
  (let* ((temp fitness-array)
               (best (car (get-best temp 0 '())))
               (fitness (calculate-fitness best 0))
               (total (stochastic-calc fitness-array 0))
               (average-fit (quotient total population-size)))
  (if (eqv? (remainder counter updateFrequency) 0)
      (begin
        (display "On the ") (display counter) (display " generation. ") (newline)
        (display "  The best individual is: ") (display best) (display ". With a fitness of ") (display fitness) (newline)
        (display "  The average fitness is ") (display average-fit) (display ".") (newline)))
  (if continue
      (begin
        (update-generation population)
;;        (set! fitness-array '())
;;        (population-fitness population)
        (evolve (check-fitness fitness-array)))
      (begin
        (display "Target chromosome achieved: ") (display best) (display " on the ") (display counter) (display " Generation.") (newline)
        (display population)))))

;; Checks population fitnesses to desired outcome fitness
(define (check-fitness lst)
  (if (> counter stopper)
      #f
      (if (null? lst)
          #t
          (if (= (car (car lst)) target-fitness)
              #f
              (check-fitness (cdr lst))))))

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

;; Breeds a new generation from the current population
(define (update-generation population)
  (set! counter (+ counter 1))
  (set! fitness-array '())
  (population-fitness population)
  (set! population '())
  (let ((temp fitness-array))
    (set! stochastic-array '())
    (let* ((total (stochastic-calc temp 0)))
      (display population) (newline)
      (breed 0 total))))

;; Creates all children
(define (breed count total)
  (display count) (display "  ") (display population-size) (display "   ") (display population) (newline)
  (if (< count population-size)
      (let* ((father-int (+ (random total) 1))
             (mother-int (+ (random total) 1))
             (father (get-chromo father-int stochastic-array))
             (mother (get-chromo mother-int stochastic-array))
             (child (crossover mother father))
             (child (mutation child)))
        (display child)
        (set! population (append population (list child)))
        (breed (+ count 1) total))))

;; Searches the fitness probabilty ranges for the parent
(define (get-chromo int array)
  (let ((firstLot (car (car array)))
        (secondLot (car (cadr array))))
;;    (display "FirstLot: ") (display firstLot) (newline)
;;    (display "SecondLot: ") (display secondLot) (newline)
;;    (display "Target: ") (display int) (newline)
    (if (or (= int firstLot) (< int firstLot))
        (cadr (car array))
        (if (and (>= secondLot int) (> int firstLot))
            (cadr (cadr array))
            (get-chromo int (cdr array))))))

;; Creates a child by crossing two parents together
(define (crossover mother father)
  (let
      ((distance (random (length mother)))
       (side (random 2))) ;side will either be 1 or 0)
    (if (> side 0) ;if side is 1
        (append (list-head mother distance) (list-tail father distance))
        (append (list-head father distance) (list-tail mother distance)))))

;; May mutate a chromosome, possibly more than once
(define (mutation lst)
  (map
   (lambda (x)
     (if (eqv? (random 300) 0)
         (flip x)
         x))
   lst))

;; Flips a 1 to 0 or a 0 to 1
(define (flip int)
  (if (> int 0)
      0
      1))

;; Creates the fitness probabilty ranges array
(define (stochastic-calc lst total)
  (if (not (null? lst))
      (begin
        (let* ((fitness (car (car lst)))
               (chrom (cdr (car lst)))
               (lst (cdr lst))
               (new-total (+ fitness total)))
          (set! stochastic-array
                (append stochastic-array
                        (list (list new-total (car chrom)))))
          (stochastic-calc lst new-total)))
      total))

;; Finds the best chromosome
(define (get-best lst int best)
  (if (null? lst)
      best
      (if (> int (car (car lst)))
          (get-best (cdr lst) int best)
          (get-best (cdr lst) (car (car lst)) (cdr (car lst))))))

(run 4 10 5)

;;(cd "/Users/tonyknapp/git/genetic-scheming/src")
;;(load "genetics.ss")