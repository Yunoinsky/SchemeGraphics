(load-shared-object "./mGE/build/windows/x64/release/mGE.dll")
(load "miniIO.ss")
(load "matrix.so")

(import (matrix))

(define width 800)
(define height 600)

(define test-foreign (foreign-procedure "test" (unsigned) unsigned))

(define engine-init (foreign-procedure "engineInit" (int int string) ptr))

(define engine-term (foreign-procedure "engineTerm" () void))

(define engine-stop? (foreign-procedure "engineProcess" () boolean))

(define engine-input (foreign-procedure "processInput" () unsigned))

(define background-init (foreign-procedure "backgroundInit" (int int string string boolean) unsigned))

(define background-draw (foreign-procedure "backgroundDraw" (u8* int int) void))

(define mGE
  (lambda (init-width init-height name vert frag use-filename init-call loop-call)
    (set! width init-width)
    (set! height init-height)
    (engine-init width height name)
    (background-init width height vert frag use-filename)
    (let ([canvas (make-bytevector (* width height 3) 200)]
          [states '(head)])
      (init-call canvas states)
      (do ([input 0 (engine-input)])
            ((engine-stop?) (engine-term))
          (loop-call canvas states input)
          (background-draw canvas width height)))))

(define canvas-point-set!
  (lambda (canvas x y color)
    (let ([x (exact (round x))]
          [y (exact (round y))])
      (when (and (>= x 0) (< x width)
                 (>= y 0) (< y height))
        (let ([base (* (+ (* y width) x) 3)])
          (bytevector-u24-set! canvas base color (endianness big)))))))

(define canvas-point-ref
  (lambda (canvas x y)
    (let ([base (* (+ (* y width) x) 3)])
      (bytevector-u24-ref canvas base (endianness big)))))

(define canvas-line!
  (case-lambda
    [(canvas x0 y0 x1 y1 color)
     (let ([dx (- x1 x0)]
           [dy (- y1 y0)])
       (let-values
           ([(x-step y-step end-predit?)
             (if (> (abs dx) (abs dy))
                 (values (/ dx (abs dx))
                         (/ dy (abs dx))
                         (if (positive? dx)
                             (lambda (x y) (>= x x1))
                             (lambda (x y) (<= x x1))))
                 (values (/ dx (abs dy))
                         (/ dy (abs dy))
                         (if (positive? dy)
                             (lambda (x y) (>= y y1))
                             (lambda (x y) (<= y y1)))))])
         (do ([x x0 (+ x x-step)]
              [y y0 (+ y y-step)])
             ((end-predit? x y))
           (canvas-point-set! canvas x y color))))]
    [(canvas p0 p1 color)
     (canvas-line! canvas (car p0) (cdr p0) (car p1) (cdr p1) color)]))

(define canvas-rect-line!
  (lambda (canvas x0 y0 x1 y1 color)
    (canvas-line! canvas x0 y0 x0 y1 color)
    (canvas-line! canvas x0 y0 x1 y0 color)
    (canvas-line! canvas x1 y1 x0 y1 color)
    (canvas-line! canvas x1 y1 x1 y0 color)))

(define canvas-tri-line!
  (lambda (canvas x0 y0 x1 y1 x2 y2 color)
    (canvas-line! canvas x0 y0 x1 y1 color)
    (canvas-line! canvas x2 y2 x1 y1 color)
  (canvas-line! canvas x0 y0 x2 y2 color))
  #;(lambda (canvas x0 y0 x1 y1 x2 y2 color)
    (let-values ([(p0 p1 p2)
                  (apply values (list-sort (lambda (a b)
                           (> (cdr a) (cdr b)))
                         (list (cons x0 y0)
                               (cons x1 y1)
                               (cons x2 y2))))])
      (canvas-line! canvas p0 p2 #xff2211)
      (canvas-line! canvas p1 p2 #x11ff22)
      (canvas-line! canvas p1 p0 #x11ff22))))

(define ratio-interpolation
  (lambda (x0 x1 alpha)
    (+ x0 (* alpha (- x1 x0)))))

(define point-interpolation
  (lambda (p0 p1 alpha)
    (cons (ratio-interpolation (car p0) (car p1))
          (ratio-interpolation (cdr p0) (cdr p1)))))

(define values-pair-sort
  (lambda (a b)
    (if (> a b)
        (values b a)
        (values a b))))

(define barcentric
  (lambda (x y x0 y0 x1 y1 x2 y2)
    (let ([v1 (vector (- x1 x0)
                      (- x2 x0)
                      (- x0 x))]
          [v2 (vector (- y1 y0)
                      (- y2 y0)
                      (- y0 y))])
      (let ([vec (vector-cross v1 v2)])
        (if (< (abs (vector-ref vec 2)) 1)
            (vector -1 1 1)
            (let ([u (/ (vector-ref vec 0)
                        (vector-ref vec 2))]
                  [v (/ (vector-ref vec 1)
                        (vector-ref vec 2))])
              (vector (- 1.0 u v) u v)))))))

(define in-triangle?
  (lambda (x y x0 y0 x1 y1 x2 y2)
    (let ([bc (barcentric x y x0 y0 x1 y1 x2 y2)])
      (and (>= (vector-ref bc 0) 0.0)
           (>= (vector-ref bc 1) 0.0)
           (>= (vector-ref bc 2) 0.0)))))

(define clamp
  (lambda (x minimum maximum)
    (max (min x maximum) minimum)))

(define canvas-tri-fill!
  (lambda (canvas x0 y0 x1 y1 x2 y2 color)
    (let ([left (min x0 x1 x2 0)]
          [right (max x0 x1 x2 (- width 1))]
          [bottom (min y0 y1 y2 0)]
          [top (max y0 y1 y2 (- height 1))])
      (do ([x left (+ x 1)])
          ((> x right))
        (do ([y bottom (+ y 1)])
            ((> y top))
          (when (in-triangle? x y x0 y0 x1 y1 x2 y2)
            (canvas-point-set! canvas x y color))))))
  #;(lambda (canvas x0 y0 x1 y1 x2 y2 color)
    (let-values ([(p0 p1 p2)
                  (apply values (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                                           (list (cons x0 y0)
                                                 (cons x1 y1)
                                                 (cons x2 y2))))])
      (let ([total-height (- (cdr p2) (cdr p0))]
            [seg-height (- (cdr p1) (cdr p0))]
            [pl p0]
            [ph p1])
        (do ([y 0 (+ y 1)])
            ((= y total-height))
          (when (= y seg-height)
            (set! seg-height (- (cdr p2) (cdr p1)))
            (set! pl p1)
            (set! ph p2))
          (let ([alpha (/ y total-height)]
                [beta (/ (- y (- (cdr pl) (cdr p0))) seg-height)])
            (let-values ([(x-start x-end)
                          (values-pair-sort
                           (ratio-interpolation
                            (car p0)
                            (car p2)
                            alpha)
                           (ratio-interpolation
                            (car pl)
                            (car ph)
                            beta))])
              (do ([x (exact (floor x-start)) (+ x 1)])
                  ((>= x x-end))
                (canvas-point-set! canvas x (+ y (cdr p0)) color)))))))))

(define canvas-face-line!
  (lambda (canvas verts face color fact)
    (let ([v0 (vector-ref verts (matrix-ref face 0 0))]
          [v1 (vector-ref verts (matrix-ref face 1 0))]
          [v2 (vector-ref verts (matrix-ref face 2 0))])
      (let ([x0 (* (/ (+ (vector-ref v0 0) 1) 2) fact)]
            [x1 (* (/ (+ (vector-ref v1 0) 1) 2) fact)]
            [x2 (* (/ (+ (vector-ref v2 0) 1) 2) fact)]
            [y0 (* (/ (+ (vector-ref v0 1) 1) 2) fact)]
            [y1 (* (/ (+ (vector-ref v1 1) 1) 2) fact)]
            [y2 (* (/ (+ (vector-ref v2 1) 1) 2) fact)])
        (canvas-line! canvas x0 y0 x1 y1 color)
        (canvas-line! canvas x2 y2 x1 y1 color)
        (canvas-line! canvas x2 y2 x0 y0 color)))))

(define canvas-obj-line!
  (lambda (canvas obj color fact)
    (let ([faces (cadr obj)]
          [vertices (car obj)])
      (vector-for-each
       (lambda (face)
         (canvas-face-line! canvas vertices face color fact))
       faces))))

(define canvas-face-random-fill!
  (lambda (canvas verts face fact)
    (let ([v0 (vector-ref verts (matrix-ref face 0 0))]
          [v1 (vector-ref verts (matrix-ref face 1 0))]
          [v2 (vector-ref verts (matrix-ref face 2 0))])
      (let ([x0 (* (/ (+ (vector-ref v0 0) 1) 2) fact)]
            [x1 (* (/ (+ (vector-ref v1 0) 1) 2) fact)]
            [x2 (* (/ (+ (vector-ref v2 0) 1) 2) fact)]
            [y0 (* (/ (+ (vector-ref v0 1) 1) 2) fact)]
            [y1 (* (/ (+ (vector-ref v1 1) 1) 2) fact)]
            [y2 (* (/ (+ (vector-ref v2 1) 1) 2) fact)])
        (canvas-tri-fill! canvas x0 y0 x1 y1 x2 y2 (random #x1000000))))))
    
(define canvas-obj-random-fill!
  (lambda (canvas obj fact)
    (let ([faces (cadr obj)]
          [vertices (car obj)])
      (vector-for-each
       (lambda (face)
         (canvas-face-random-fill! canvas vertices face fact))
       faces))))
