(define string-split
  (lambda (str sep)
    (let ([len (string-length str)]
          [start 0]
          [ss '()]
          [sep (string->list sep)]
          [outsub #t])
      (do ([i 0 (+ i 1)]
           [lastoutsub #t outsub])
          ((= i len) (if outsub
                         (reverse ss)
                         (reverse (cons (substring str start i) ss))))
        (set! outsub (member (string-ref str i) sep))
        (if (and (not outsub) lastoutsub)
            (set! start i)
            (when (and (not lastoutsub) outsub)
              (set! ss (cons (substring str start i) ss))))))))

(define string-split-unit
  (lambda (str sep)
    (let ([len (string-length str)]
          [start 0]
          [ss '()]
          [sep (string->list sep)])
      (do ([i 0 (+ i 1)])
          ((= i len) (reverse (cons (substring str start i) ss)))
        (when (member (string-ref str i) sep)
          (set! ss (cons (substring str start i) ss))
          (set! start (+ i 1)))))))

(define string-trim
  (case-lambda
    [(str sep)
     (let ([sep (string->list sep)]
           [len (string-length str)])
       (if (= len 0)
           ""
           (let* ([start
                   (do ([i 0 (+ i 1)])
                       ((or (= i len)
                            (not (member (string-ref str i) sep))) i))]
                  [end
                   (do ([i (- len 1) (- i 1)])
                       ((or (= i start)
                            (= i 0)
                            (not (member (string-ref str i) sep))) (+ i 1)))])
             (if (< start end)
                 (substring str start end)
                 ""))))]))

(define read-csv-line
  (lambda (port)
    (let ([str (get-line port)])
      (if (eof-object? str)
          (eof-object)
          (if (or (eqv? str "")
                  (eqv? (string-ref str 0) #\#)
                  (eqv? (string-trim str " ,") ""))
              (read-csv-line port)
              (string-split str ", "))))))

(define vertex-parse
  (lambda (vert-list)
    (list->vector
     (map (lambda (num) (string->number num))
          vert-list))))

;; 得到一个矩阵，每个列向量是一个顶点三元组
;; [v vt vn]
;; 如果只有v和vn，则vt保留为[#f]

(define face-parse
  (lambda (face-list)
    (list->vector
     (map (lambda (v)
            (list->vector
             (map (lambda (num) (if (eqv? num "")
                                    #f
                                    (- (string->number num) 1)))
                  (string-split-unit v "/"))))
          face-list))))

(define read-obj
  (lambda (filename)
    (let ([fp (open-input-file filename)]
          [vertex '()]
          [face '()])
      (do ([line (read-csv-line fp) (read-csv-line fp)])
          ((eof-object? line) (list (list->vector (reverse vertex))
                                    (list->vector (reverse face))))
        (case (car line)
          ["v" (set! vertex (cons (vertex-parse (cdr line)) vertex))]
          ["f" (set! face (cons (face-parse (cdr line)) face))]
          [else 'todo])))))
