(define 2ipi (* 0+4i (asin 1)))

(numerator (rationalize (/ 15 25) 1/10000))

(define fft
  (lambda (x)
    (let ([N (vector-length x)])
      (let ([W (let ([halfN (div0 N 2)]
                     [W1 (exp (- (/ 2ipi N)))])
                 (let ([Ws (make-vector halfN)])
                   (do ([i 0 (+ i 1)]
                        [Wi 1.0 (* Wi W1)])
                       ((= i halfN))
                     (vector-set! Ws i Wi))
                   (lambda (k)
                     (if (>= k halfN)
                         (- (vector-ref Ws (- k halfN)))
                         (vector-ref Ws k)))))])
        (define reverse-ind
          (let ([levels (bitwise-length (- N 1))])
            (lambda (ind)
              (bitwise-reverse-bit-field ind 0 levels))))
        (let ([X (list->vector (map (lambda (a)
                                      (vector-ref x (reverse-ind a)))
                                    (iota N)))]
              [X-temp (make-vector N)])
          (let fft-loop ([X X]
                         [X-temp X-temp]
                         [size 2]
                         [groups (div N 2)])
            (if (= groups 0)
                X-temp
                (do ([i 0 (+ i 1)])
                    ((= i groups) (fft-loop X-temp X (* size 2) (div groups 2)))
                  (do ([j 0 (+ j 1)])
                      ((= j size))
                    (vector-set! X-temp
                                 (+ (* i size) j)
                                 (+ (vector-ref X (+ (* i size) j))
                                    (* (vector-ref X (+ (* i size) (mod (+ j (div size 2)) size)))
                                       (W (div (* j 8) size))))))))))))))

(define v
  (let ([v (make-vector (expt 2 15))])
    (do ([i 0 (+ i 1)])
        ((= i (expt 2 15)) v)
      (vector-set! v i (sin (* i 0.01))))))



