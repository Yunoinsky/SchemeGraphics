(load "mGE.ss")

(define init
  (lambda (canvas states)
    (bytevector-fill! canvas 20)
    (canvas-tri-fill! canvas 10 70 50 160 70 80 #xff2200)
    (canvas-tri-fill! canvas 180 50 150 1 70 180 #xfefaff)

    (canvas-face-random-fill! canvas '#(#(10 20 30)
                                        #(30 70 60)
                                        #(100 50 10))
                              '#(#(0) #(1) #(2))
                              1.0)
                              
    (canvas-obj-random-fill! canvas (read-obj "african_head.obj") 400.0)))


(read-obj "african_head.obj")

(define loop
  (lambda (canvas states input)
    #t))

(mGE 400 300 "Good" "./mGE/src/Vert.glsl" "./mGE/src/Frag.glsl" #t init loop)


