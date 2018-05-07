#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10)
(define SIZE 50)
(define SCENE-SIZE (* WIDTH SIZE))

(define landscape
  (empty-scene SCENE-SIZE SCENE-SIZE 'black))

(define landscape0
  (empty-scene 300 300 'black))

(define (attacker-img color)
  (rotate 180 (triangle SIZE 'solid color)))

(define colors
  (vector "green" "red" "yellow" "purple" "blue"))

(define random-color
  (lambda ()
    (let ((r (random 5)))
      (vector-ref colors r))))

(define-struct shooter (x score shooting?))
(define-struct attacker (x y color))
(define-struct game (s a paused?))

(define shooter-img (triangle SIZE 'solid 'gray))

(define laser-beam
    (rectangle (/ SIZE 4) (/ (* 8 SCENE-SIZE) 10) 'solid 'red))

(define place-shooter
  (lambda (s l)
    (if (shooter-shooting? s)
        (place-image laser-beam (* SIZE (- (shooter-x s) 1/2)) (- (/ (* 9 SCENE-SIZE) 20) 0)
                     (place-image shooter-img (* SIZE (- (shooter-x s) 1/2)) (* SIZE (- 10 1/2)) l))
        (place-image shooter-img (* SIZE (- (shooter-x s) 1/2)) (* SIZE (- 10 1/2)) l))))

(define place-attacker
  (lambda (a)
    (place-image (attacker-img (attacker-color a)) (* SIZE (- (attacker-x a) 1/2)) (* SIZE (- (attacker-y a) 1/2)) landscape)))

(define gen-attacker
  (lambda ()
    (make-attacker (add1 (random 10)) 1 (random-color))))

(define render
  (lambda (g)
    (place-image (text (get-score g) 25 "green") (/ SCENE-SIZE 2) 12.5
                 (if (game-paused? g) (place-image (text "PAUSED" 50 "green")
                                                   (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
                                                   (place-shooter (game-s g) (place-attacker (game-a g))))
                     (place-shooter (game-s g) (place-attacker (game-a g)))))))

(define move-attacker
  (lambda (g)
    (if (game-paused? g) g
    (make-game (make-shooter (shooter-x (game-s g)) (shooter-score (game-s g)) #f)
               (make-attacker (attacker-x (game-a g)) (add1 (attacker-y (game-a g))) (attacker-color (game-a g))) #f))))

(define move-shooter
  (lambda (g k)
    (cond
      [(key=? k "escape")
       (if (game-paused? g)
           (make-game (game-s g) (game-a g) #f)
           (make-game (game-s g) (game-a g) #t))]
      [(game-paused? g) g]
      [else
       (cond
         [(key=? k "right")
          (if (= 10 (shooter-x (game-s g))) g
              (make-game (make-shooter (add1 (shooter-x (game-s g))) (shooter-score (game-s g)) #f)
                         (game-a g) #f))]
         [(key=? k "left")
          (if (= 1 (shooter-x (game-s g))) g
              (make-game (make-shooter (sub1 (shooter-x (game-s g))) (shooter-score (game-s g)) #f)
                         (game-a g) #f))]
         [(key=? k "e")
              (make-game (game-s g) (make-attacker (attacker-x (game-a g)) 10 (attacker-color (game-a g))) #f)]
         [(key=? k " ")
          (if (= (shooter-x (game-s g)) (attacker-x (game-a g)))
              (make-game (make-shooter (shooter-x (game-s g)) (add1 (shooter-score (game-s g))) #t)
                         (gen-attacker) #f)
              (make-game (make-shooter (shooter-x (game-s g)) (shooter-score (game-s g)) #t)
                         (game-a g) #f))]
         [else g])])))

(define lost?
  (lambda (g)
      (= (attacker-y (game-a g)) 10)))

(define get-score
  (lambda (g)
    (string-append "SCORE: " (number->string (shooter-score (game-s g))))))

(define lost
  (lambda (g)
    (place-image (text "GAME OVER" 50 "green") (/ SCENE-SIZE 2) (/ SCENE-SIZE 3)
                 (place-image (text (get-score g) 35 "green")
                              (/ SCENE-SIZE 2) (/ SCENE-SIZE 3/2) landscape))))

(define game-main 
  (lambda (g r)
    (big-bang g
      [name "Space Invaders"]
      [to-draw render]
      [on-tick move-attacker r]
      [stop-when lost? lost]
      [on-key move-shooter])))

(define start-game
  (lambda (r)
    (game-main (make-game (make-shooter 5 0 #f) (gen-attacker) #f) r)))

(define game-starter
  (lambda (g k)
    (cond
      [(key=? k "e") (start-game 0.5)]
      [(key=? k "m") (start-game 0.3)]
      [(key=? k "h") (start-game 0.2)])))

(define start 
  (lambda (g)
    (place-image (text "PRESS E FOR EASY" 25 "white") 150 50
                (place-image (text "PRESS M FOR MEDIUM" 25 "white") 150 150
                              (place-image (text "PRESS H FOR HARD" 25 "white") 150 250 landscape0)))))

(define main
  (lambda (g)
    (big-bang g
      [name "Space Invaders Launcher"]
      [to-draw start]
      [on-key game-starter])))

(main 0)      
