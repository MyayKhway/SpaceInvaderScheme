(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders Tank


;; Constants:

;; for Tank
(define WIDTH  300)
(define HEIGHT 500)

(define TANK-SPEED 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

;; for Missiles

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-HEIGHT (image-height TANK))
(define MISSILE-SPEED 10)
(define MISSILE (ellipse 5 15 "solid" "yellow"))

;; For Invaders

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define HIT-RANGE 10)
(define INVADE-RATE 100)
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define MEANING-OF-LIFE 42)
(define GAME-OVER-SCREEN (place-image (text "GAME OVER" 40 "Red")
                                      (/ WIDTH 2)
                                      (/ HEIGHT 2)
                                      BACKGROUND))

;; Data Definitions:

;; Data Definitions:

;;TANKKKKKKKKKKKKKKKKKKK
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;;INVADERRRRRRRRRRRRRR
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListofInvader is one of
;; - empty
;; - (cons Invader ListofInvader)
;; interp, Arbitary Number of invaders
(define loi0 empty)
(define loi1 (cons I1 empty))
(define loi3 (cons I2 loi1))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (...
               (fn-for-invader(first loi))
               (fn-for-loi(rest loi)))]))

;; Template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; Compound: (cons invader ListofInvader)
;; Reference: (first loi) is Invader
;; Self-reference: (rest loi) is ListofInvader


;;MISSILESSSSSSSSSSSSS
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListofMissile is one of
;; - empty
;; - (cons Missile ListofMissile)
;; interp, Arbitary Number of missiles
(define lom0 empty)
(define lom1 (cons M1 empty))
(define lom3 (cons M2 lom1))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (...
               (fn-for-missile(first lom))
               (fn-for-lom(rest lom)))]))

;; Template rules used:
;; one of: 2 cases
;; atomic distinct: empty
;; Compound: (cons Missile ListofMissile)
;; Reference: (first lom) is Nissile
;; Self-reference: (rest lom) is ListofMissile

;; Gameeeeeeeeeeeeeee
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;  with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main t)
  (big-bang (make-game empty empty (make-tank t 1))                 ; Game
    (on-tick   next-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (stop-when Game-Over? last-picture)      ; Game -> Boolean
    (on-key    handle-keys)))    ; Game KeyEvent -> Game

;; Game -> Game
;; Produces next game state
;;behaviour
(define (next-game g)
  (make-game (delete-invader (next-invaders (game-invaders g)) (game-missiles g)) (next-missiles (game-missiles g)) (next-tank (game-tank g))))

;; Tank -> Tank
;; produce the next tank according to the dir field and TANK SPEED constant
(check-expect (next-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 20 -1)) (make-tank (- 20 TANK-SPEED) -1))
(check-expect (next-tank (make-tank 20 0)) (make-tank 20 0))
;(define (next-tank t) (make-tank 0 1))  ; stub

;use template from tank
(define (next-tank t)
  (cond [(< (tank-x t) 0) (make-tank 0 (tank-dir t))]
        [(> (tank-x t) WIDTH) (make-tank WIDTH (tank-dir t))]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; ListofMissile -> ListofMissile
;; move all the missiles in the lom after deleting those off screen and hit an invader
(define (next-missiles lom)
  (move-missiles (on-screen lom)))

;; ListofMissile -> ListofMissile
;; delete the missile that is not on screen

(define (on-screen lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) 0)
             (on-screen (rest lom))
             (cons (first lom) (on-screen (rest lom))))]))

;; ListofMissile -> ListofMissile
;; move all missiles in the lom into another missile according to MISSILE-SPEED
;; Assume that all missiles in lom are considered not hit with an invader and on the screen

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons
               (move-missile (first lom))
               (move-missiles (rest lom)))]))

;; Missile -> Missile
;; return the next Missile according to MISSILE-SPEED
(check-expect (move-missile (make-missile 30 100)) (make-missile 30 (- 100 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 200 10)) (make-missile 200 (- 10 MISSILE-SPEED)))

;(define (move-missile m) (make-missile 0 0))  ; stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListofMissile ListofInvader -> ListofMissile
;; Take a lom and return an lom without the missiles that hit an invader
(check-expect (delete-missile empty loi3) empty)
(check-expect (delete-missile lom3 empty) lom3)
(check-expect (delete-missile lom3 loi1) (cons M1 empty))
(check-expect (delete-missile (cons (make-missile 150 50) lom3) (cons (make-invader 10 10 -1) loi1))
              (cons (make-missile 150 50) (cons M1 empty)))

;(define (delete-missile lom loi) empty)  ; stub

(define (delete-missile lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (missile-hit? (first lom) loi)
             (delete-missile (rest lom) loi)
             (cons (first lom) (delete-missile (rest lom) loi)))]))

;; Missile ListofInvader -> Boolean
;; return true if given missile hit one of the invader in given loi
(check-expect (missile-hit? M1 empty) false)
(check-expect (missile-hit? M1 loi3) false)
(check-expect (missile-hit? M2 loi3) true)
(check-expect (missile-hit? M3 loi1) true)

;(define (missile-hit? m loi) false)  ; stub

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (hit? m (first loi))
             true
             (missile-hit? m (rest loi)))]))

;; Missile Invader -> Boolean
;; Return true if given Missile and Invader hit each other
(check-expect (hit? M1 I1) false)
(check-expect (hit? M2 I1) true)
(check-expect (hit? M3 I1) true)

;(define (hit? m i) false)  ; stub
(define (hit? m i)
  (<= (integer-sqrt (+ (sqr (- (missile-x m) (invader-x i))) (sqr (- (missile-y m) (invader-y i))))) HIT-RANGE))

;; ListofInvader ListofMissile -> ListofInvader
;; produce next loi after randoming spawning one and moving all invaders and deleteing those hit with missiles

;(define (next-invaders loi) empty)  ; stub

(define (next-invaders loi)
  (move-invaders (random-spawn loi)))

;; ListofInvader -> ListofInvader
;; take a loi and spawn a new invader randomly

;(define (random-spawn loi) empty)  ; stub

(define (random-spawn loi)
  (if (= (random INVADE-RATE) MEANING-OF-LIFE)
      (cons (make-invader (random WIDTH) 0 1) loi)
      loi))

;; ListofInvader -> ListofInvader
;; take an loi and move all invaders in accordance with x and y speed and dx
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders loi1) (cons (move-invader I1) empty))
(check-expect (move-invaders loi3) (cons (move-invader I2) (cons (move-invader I1) empty)))
;(define (move-invaders loi) empty)  ; stub

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons
               (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; take an invader and move it in dx direction INVADER-SPEED-X and INVADER-SPEED-Y
(check-expect (move-invader (make-invader 150 10 1)) (make-invader (+ 150 1) (+ 10 1) 1))
(check-expect (move-invader (make-invader WIDTH 50 1)) (make-invader (- WIDTH 1) (+ 50 1) -1))
(check-expect (move-invader (make-invader 0 50 -1)) (make-invader 1 (+ 50 1) 1))
(check-expect (move-invader (make-invader 150 10 -1)) (make-invader (- 150 1) (+ 10 1) -1))

;(define (move-invader i) i)  ; stub

(define (move-invader i)
  (cond [(<= (invader-x i) 0) (make-invader (+ (invader-x i) 1) (+ (invader-y i) 1) 1)]
        [(>= (invader-x i) WIDTH) (make-invader (- (invader-x i) 1) (+ (invader-y i) 1) -1)]
        [else (make-invader (+ (invader-x i) (* (invader-dx i) 1)) (+ (invader-y i) 1) (invader-dx i))]
        ))

;; ListofInvader ListofMissile -> ListofInvader
;; take an loi and return an loi without those that are hit with missiles in lom
(check-expect (delete-invader empty lom3) empty)
(check-expect (delete-invader loi3 empty) loi3)
(check-expect (delete-invader loi1 lom3) empty)
(check-expect (delete-invader loi3 lom3) (cons I2 empty))
(check-expect (delete-invader (cons I3 (cons I2 (cons I1 empty))) (cons M2 empty))
              (cons I3 (cons I2 empty)))
;(define (delete-invader loi lom) empty)  ; stub

(define (delete-invader loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit-invader? (first loi) lom)
             (delete-invader (rest loi) lom)
             (cons (first loi) (delete-invader (rest loi) lom)))]))

;; Invader ListofMissile -> Boolean
;; return true if the given Invader hit a missile in ListofMissile otherwise false
(check-expect (hit-invader? I1 empty) false)
(check-expect (hit-invader? I1 lom1) false)
(check-expect (hit-invader? I1 lom3) true)
(check-expect (hit-invader? I1 (cons M1 empty)) false)

;(define (hit-invader? i lom) false)  ; stub

(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (if (hit? (first lom) i)
             true
             (hit-invader? i (rest lom)))]))

;; Game -> Image
;; render Tanks and Missiles

(define (render-game g)
  (render-loi (game-invaders g) (render-lom (game-missiles g)
                                            (render-tank (game-tank g)))))

;; Tank -> Image
;; render Tank onto BACKGROUND

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; ListofMissile -> Image
;; render ListofMissiles onto existing game with tank

(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-lom (rest lom) img))]))

;; ListofInvaders Image -> Image
;; render LisfofInvaders onto existing game with missiles and tank

(define (render-loi loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-loi (rest loi) img))]))

;; Game KeyEvent -> Game
;; if left is pressed move tank left, right right, and spcae shoot missile
(check-expect (handle-keys G0 " ") (make-game empty (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT)) empty) (game-tank G0)))
(check-expect (handle-keys G0 "left") (make-game empty empty (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (handle-keys G0 "right") (make-game empty empty (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (handle-keys G0 "a") G0)

(define (handle-keys g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT)) (game-missiles g)) (game-tank g))]
        [else 
         g]))

;; Game -> Boolean
;; return true if game is over
(check-expect (Game-Over? G0) false)
(check-expect (Game-Over? G1) false)
(check-expect (Game-Over? (make-game (cons (make-invader 150 HEIGHT -1) empty) empty T0)) true)

;(define (Game-Over? g) false)  ; stub

(define (Game-Over? g)
  (landed? (game-invaders g)))

;; ListofInvaders -> Boolean
;; return true if an invader has landed otherwise false
(check-expect (landed? empty) false)
(check-expect (landed? loi3) true)
(check-expect (landed? (cons I1 (cons (make-invader 150 HEIGHT 1) empty))) true)
(check-expect (landed? (cons (make-invader 150 (+ HEIGHT 1) -1) (cons I1 empty))) true)

;(define (landed? loi) false) ; stub
(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (landed? (rest loi)))]))

(define (last-picture g)
  GAME-OVER-SCREEN)
