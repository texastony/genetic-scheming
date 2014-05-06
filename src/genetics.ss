(define population '()) ;; Holds the last generation
(define fitness-array '()) ;; Holds the each member of the last generation with paired with their fitness
(define counter 0) ;; Keeps track of what generation we are on
(define updateFrequency 0) ;; Determines how often to print last generation statistics
(define population-size 0) ;; Dictates how big the population should be
(define stochastic-array '()) ;; Holds the last generation paired with 
(define target-fitness 0) ;; The best possible fitness
(define stopper 1000000000) ;; A limit on how many generations to use

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
        (evolve (check-fitness fitness-array)))
      (begin
        (display "Target chromosome achieved: ") (display best) (display " on the ") (display counter) (display " Generation.") (newline)
        (display "  The average fitness is ") (display average-fit) (display ".") (newline)))))

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
(define (update-generation lst)
  (set! counter (+ counter 1))
  (set! fitness-array '())
  (population-fitness lst)
  (set! population '())
  (let ((temp fitness-array))
    (set! stochastic-array '())
    (let* ((total (stochastic-calc temp 0)))
      (breed 0 total))))

;; Creates all children
(define (breed count total)
  (if (< count population-size)
      (let* ((father-int (+ (random total) 1))
             (mother-int (+ (random total) 1))
             (father (get-chromo father-int stochastic-array))
             (mother (get-chromo mother-int stochastic-array))
             (child (crossover mother father))
             (child (mutation child)))
        (set! population (append population (list child)))
        (breed (+ count 1) total))))

;; Searches the fitness probabilty ranges for the parent
(define (get-chromo int array)
  (let ((firstLot (car (car array)))
        (secondLot (car (cadr array))))
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

(run 32 100 10)
;;(run Size-of-Chromosome Size-of-population frequency-of-information)


;;(cd "/Users/tonyknapp/git/genetic-scheming/src")
;;(load "genetics.ss")