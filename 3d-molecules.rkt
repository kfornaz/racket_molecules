#lang racket

(require plot)
(require (only-in 2htdp/batch-io
                  read-csv-file))
(require (only-in srfi/1
                  zip))

(provide plot-by-name)
(provide get-molecules-names)

(define (get-molecules-names arq)
  (define raw-data (read-csv-file arq)) ; list of lines
  (rest (first raw-data)) ; list of molecules's names
  )

(define (search-index datum in-list acc)
  (cond [(string=? datum (first in-list)) acc]
        [else (search-index datum (rest in-list) (add1 acc))]))

(define (molecule-searcher molecule-list)
  (lambda (molecule-name)
    (search-index molecule-name molecule-list 0)))


(define (lets-do-this arq
                      ind1 ind2 ind3
                      nom1 nom2 nom3)
  (define raw-data (read-csv-file arq)) ; list of lines
  (define molecules-names (rest (first raw-data))) ; list of molecules's names
  (define stars-names (rest (first (apply zip raw-data)))) ; list of columns
  
  ; Converts to numbers and discards the first element
  (define (map-list-numbers list-of-strings)
    (map string->number (rest list-of-strings)))
  
  (define data (map map-list-numbers (rest raw-data))) ; actual data - numbers only
  (define data-by-columns (apply zip data)) ; actual data - numbers only by columns
  
  (define (for-molecule molecule)
    (search-index molecule molecules-names 0))
  ; gets the nth molecule
  (define (get-molecule-ind ind)
    (list-ref data-by-columns ind))
  
  ; Gets a columns of observations of a given molecule
  (define (get-molecule-data molecule)
    (list-ref data-by-columns (for-molecule molecule)))
  
  ; Gets the list of 3d points formed by 3 molecules (given by indexes)
  (define (get-3dpoint-from-molecules ind1 ind2 ind3)
    (zip molecules-names (list-ref data-by-columns ind1) (list-ref data-by-columns ind2) (list-ref data-by-columns ind3)))
  
  ; removes any star which doesn't have all molecules observed
  (define (remove-zeroes lst)
    (filter (λ (x) (not (or (zero? (list-ref x 1))
                            (zero? (list-ref x 2))
                            (zero? (list-ref x 3))))) lst))
  
  (define (get-3d-points-to-plot ind1 ind2 ind3)
    (map rest (remove-zeroes (get-3dpoint-from-molecules ind1 ind2 ind3))))
  
  (define (min-list lst)
    (foldr min (first lst) lst))
  (define (max-list lst)
    (foldr max (first lst) lst))
  
  (define (get-min lst)
    (define minim (log (min-list lst)))
    (exp (- minim
            (/ (- (log (max-list lst))
                  minim)
               10))))
  (define (get-max lst)
    (define maxim (log (max-list lst)))
    (exp (- maxim
            (/ (- (log (min-list lst))
                  maxim)
               10))))
  
  
  (define points-to-plot (get-3d-points-to-plot ind1 ind2 ind3))
  (displayln (format "Plotando ~a pontos" (length points-to-plot)))
  (define transposed-points (apply zip points-to-plot))
  (define min-z (get-min (list-ref transposed-points 2)))
  (define (z-to-base-points point)
    (lines3d (list point
                   (list (first point) (second point) min-z))
             #:alpha 0.3))
  (define things-to-plot (list (map z-to-base-points points-to-plot)
                               (points3d points-to-plot)))
  (plot3d things-to-plot
          #:x-min (get-min (list-ref transposed-points 0))
          #:x-max (get-max (list-ref transposed-points 0))
          #:y-min (get-min (list-ref transposed-points 1))
          #:y-max (get-max (list-ref transposed-points 1))
          #:z-min (get-min (list-ref transposed-points 2))
          #:z-max (get-max (list-ref transposed-points 2))
          #:x-label nom1
          #:y-label nom2
          #:z-label nom3))

(define (plot-by-name arq mol1 mol2 mol3)
  (define for-molecule (molecule-searcher (get-molecules-names arq)))
  (lets-do-this arq
                (for-molecule mol1) (for-molecule mol2) (for-molecule mol3)
                mol1 mol2 mol3))