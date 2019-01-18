#lang racket

(require plot)
(require 2htdp/batch-io)

; símbolos de âncora disponíveis em
; http://docs.racket-lang.org/plot/contracts.html#%28def._%28%28lib._plot%2Futils..rkt%29._anchor%2Fc%29%29

; símbolos de ponto disponíveis em
; http://docs.racket-lang.org/plot/contracts.html#%28def._%28%28lib._plot%2Futils..rkt%29._known-point-symbols%29%29

(define POINTSIZE 14)
(define TITLE "             Column Densities of NH₂CHO and HNCO")
(set! TITLE  "                Column Densities of NH₂CHO and HNCO\n")
(define XLABEL "NH₂CHO [cm⁻²]")
(define YLABEL "HNCO [cm⁻²]")

;(define data (read-csv-file "arquivo.csv"))

(define (process-raw-line line)
  (define x (string->number (second line)))
  (define y (string->number (first line)))
  (define label (third line))
  (cond [(= (length line) 5)
         (list (list x y)
               label
               (string->symbol (fourth line))
               (string->symbol (fifth line)))]
        [else (list (list x y) label)]))

;(define processed-data (map process-raw-line data))
;processed-data

(define (my-point-label proc-line)
  (cond [(= (length proc-line) 4)
         (point-label (first proc-line)
                      (second proc-line)
                      #:anchor (third proc-line)
                      #:point-sym (fourth proc-line)
                      #:point-size POINTSIZE
                      #:point-color (random 97))]
        [else
         (point-label (first proc-line)
                      (second proc-line)
                      #:anchor 'left
                      #:point-sym 'square
                      #:point-color (random 97)
                      #:point-size POINTSIZE)]))
;(define (inv.points proc-line)
;  (points '((0 1) '(1 3))))
;
;(define labeled-p (map my-point-label processed-data))
;(define to-plot (list labeled-p (points '((0 1) (1 3)) #:alpha 0)))
;(define my-plot (plot to-plot
;                      #:title TITLE
;                      #:x-label XLABEL
;                      #:y-label YLABEL
;                      #:x-max 7e15
;                      #:y-max 3e15))

(define (fit-result x)
  (* 15.6 (expt x 0.96)))
  

(define (plotit! filename title pdffile)
  (define plotpoints
    (list
     (function sqr 4e15 6e15)
     (function fit-result 1e13 1e16)
     (map my-point-label (map process-raw-line (read-csv-file filename)))
     (points '((0 1) (1 3)) #:alpha 0)))
  (parameterize ([plot-x-transform  log-transform]
                 [plot-y-transform  log-transform]
                 [plot-x-ticks      (log-ticks)]
                 [plot-y-ticks      (log-ticks)]
                 )
    (plot plotpoints
          #:title title
          #:x-label XLABEL
          #:y-label YLABEL
          #:x-max 1e16
          #:x-min 1e13
          #:y-max 1e16
          #:y-min 1e13
          #:out-file pdffile)))

;(plot-ps/pdf-interactive? #t)
(plot-width 600)
(plot-height 400)
(plotit! "arquivo.csv" TITLE "grafico.pdf")