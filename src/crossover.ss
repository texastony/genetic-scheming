(define (crossover mother father)
  (let
      ((distance (random (length mother)))
       (side (random 2))) ;side will either be 1 or 0)
    (display side)
    (display distance)
    (if (> side 0) ;if side is 1
        (append (list-head mother distance) (list-tail father distance))
        (append (list-head father distance) (list-tail mother distance)))))

(define (mutation lst)
  (map
   (lambda (x)
     (if (eqv? (random 300) 0)
         (flip x)
         x))
   lst))

(define (flip int)
  (if (> int 0)
      0
      1))