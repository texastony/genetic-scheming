(define fitness-array '())

(define (population-fitness population)
  (set! fitness-array
        (map
         (lambda (x)
           (list (calculate-fitness x 0) x))
         population)))

(define (calculate-fitness child fitness)
  (if (null? child)
      fitness
      (if (eqv? 1 (car child))
          (calculate-fitness (cdr child) (+ fitness 1))
          (calculate-fitness (cdr child) fitness))))
  