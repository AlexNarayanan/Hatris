;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hatristext) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")))))
;; =================================================================================================
;;
;;     ;;  ;;   ;;;   ;;;;;; ;;;;;; ;;;;;; ;;;;;;
;;     ;;  ;;  ;; ;;    ;;   ;;  ;;   ;;   ;;
;;     ;;;;;; ;;;;;;;   ;;   ;;;;;;   ;;   ;;;;;;
;;     ;;  ;; ;;   ;;   ;;   ;; ;;    ;;       ;;
;;     ;;  ;; ;;   ;;   ;;   ;;  ;; ;;;;;; ;;;;;;
;;
;;
;;==================================================================================================
;; A game about stacking hats
(require 2htdp/image)
(require 2htdp/universe)
;; =================================================================================================
;; Numerical Constants

(define SEGS 20) ; segments, horizontal pieces that the scene is broken up into
(define SEG-SIZE 40) ; size of a segment in pixels
(define WIDTH (* SEGS SEG-SIZE))
(define HEIGHT (* (* SEGS .8) SEG-SIZE))
(define (size n) (* n SEG-SIZE)) ; tile that describes location in reference to the canvas
(define easy/med-speed 1)
(define hard-speed 2)

;; Graphical Constants

(define HEAD .)
(define HAT0 .)
(define HAT1 .)
(define HAT2 .)
(define HAT3 .)
(define HAT4 .)


(define ES (rectangle WIDTH HEIGHT "solid" "sea green")) ; empty scene
(define GAME-RECTANGLE (rectangle  (size 12) (size 12) "solid" "aquamarine")) 
(define STANDBY-AREA (rectangle (size 4) (size 1.5) "solid" "aquamarine")) 
(define SCORE-AREA (rectangle (size 5) (size 3) "solid" "aquamarine"))
(define GAME-AREA (foldr (λ (x y) (place-image HEAD (size x) (size 11) y)) GAME-RECTANGLE 
                         (build-list 6 (λ (x) (+ 1 (* 2 x))))))
(define BG (place-image GAME-AREA (size 7) (size 9)
                        (place-image STANDBY-AREA (size 10) (size 1.5)
                                     (place-image SCORE-AREA (size 17) (size 9) ES))))

;; =================================================================================================
;; Data Definitions

;; A Gs [Gamestate] is one of:
;; - Ss [Startstate]
;; - Ps [Pausestate]
;; - Rs [Runstate]

;; Ss [Startstate] is one of:
;; - 'easy
;; - 'medium
;; - 'hard

(define-struct ps (active placed standby score diff))
;; Ps [Pausestate] is a struct:
;; (make-ps [List-of Hat] [List-of [List-of Hat]] [List-of Hat] Number Symbol])
;; a Ps contains the information of a Rs, and it does not ever change 
;; for the duration of the Ps

(define-struct rs (active placed standby score diff))
;; Rs [Runstate] is a struct:
;; (make-rs [List-of Hat] [List-of [List-of Hat]] [List-of Hat] Number Symbol])
;; interp: an rs represents:
;; - the current hats the player is controlling
;; - the current two hats the player is controlling
;; - the hats that are resting at the bottom of the game area, organized by column
;; - two numbers that represent the hats on standby
;; - the player's score

(define-struct hat (type column y))
;; A Hat is a struct:
;; (make-hat (Number Number Number))
;; interp: a hat represents
;; - the type of hat represented by a number between 0 and 4 inclusive
;; - which column in the scene it is (number between 0 and 20 inclusive)
;; - the y coordinate of the hat

;; =================================================================================================
;; =================================================================================================
;; Big-Bang

;; Ss -> Number
;; starts the game from the startscreen, and returns the player score when finished

(define (main gs)
    (big-bang gs
              [to-draw render]
              [on-tick update]
              [on-key keyh]
              [stop-when too-many-hats end-game-scene]))

;; =================================================================================================
;; =================================================================================================
;; Handler Functions for Big Bang

;; Gs -> Image
;; selects the appropriate render function for the current Gs

(check-expect (image? (render 'easy)) true)
(check-expect (image? (render ps1)) true)
(check-expect (image? (render rs-start-ex)) true)

(define (render gs)
    (cond
      [(symbol? gs) (render-start gs)]
      [(ps? gs) (render-pause gs)]
      [(rs? gs) (render-rs gs)]))

;; Gs -> Gs
;; selects the appropriate update function for the current Gs

(check-expect (update 'easy) 'easy)
(check-expect (update ps1) ps1)
(check-expect (update rs-start-ex) (update-rs rs-start-ex))

(define (update gs)
  (cond
    [(symbol? gs) gs]
    [(ps? gs) gs]
    [(rs? gs) (update-rs gs)]))

;; Gs KeyEvent -> Gs
;; selects the approriate KeyEvent handler for the current Gs

(check-expect (keyh rs-start-ex "p") (rs-keyh rs-start-ex "p"))
(check-expect (keyh 'easy "right") 'medium)
(check-expect (keyh ps1 "p") (unpause ps1 "p"))

(define (keyh gs k)
  (cond
    [(symbol? gs) (change-difficulty gs k)]
    [(ps? gs) (unpause gs k)]
    [(rs? gs) (rs-keyh gs k)]))

;; Gs -> Boolean
;; checks if the hats are stacked too high, only for a Rs

(check-expect (too-many-hats 'easy) false)
(check-expect (too-many-hats (make-ps empty empty empty 1 'easy)) false)
(check-expect (too-many-hats rs-start-ex) false)

(define (too-many-hats gs)
  (cond
    [(symbol? gs) false]
    [(ps? gs) false]
    [(rs? gs) (hats-too-high? (rs-placed gs))]))


;; =================================================================================================
;; Starting up the game

;; Ss -> Image
;; renders the start menu

(check-expect (image? (render-start 'medium)) true) ; STEP 3 NEVER FORGET

(define (render-start s)
  (local (;; bunch-o local stuff
          (define mid (/ WIDTH 2))
          (define p-i place-image)
          (define title-text-a (text "Welcome to Hatris" 75 "black"))
          (define title-text-b (text "Select Difficulty" 50 "black"))
          (define title-text-c (text "Down to Start" 40 "black"))
          (define title-text-d (text "Controls: left/right to move, space to switch hats" 30 "black"))
          (define title-text-e (text "down to drop, p to pause" 30 "black"))
          (define title-scene (p-i title-text-a mid (* SEG-SIZE 3)
                                   (p-i title-text-b mid (* SEG-SIZE 6)
                                        (p-i title-text-c mid (* SEG-SIZE 10)
                                             (p-i title-text-d mid (* SEG-SIZE 12)
                                                  (p-i title-text-e mid (* SEG-SIZE 13) ES))))))
          
          ;; color-diff highlights the currently selected difficulty in red
          (define (color-diff s d) 
            (if (symbol=? s d) "darkred" "black")))
    
    ;; incorporates color-diff into the overlaying of text
    (p-i (text "Wimp" 45 (color-diff s 'easy)) (* SEG-SIZE 5) (* SEG-SIZE 8)
         (p-i (text "Wumbo" 45 (color-diff s 'medium)) (* SEG-SIZE 10) (* SEG-SIZE 8)
              (p-i (text "Wdeath" 45 (color-diff s 'hard)) (* SEG-SIZE 15) (* SEG-SIZE 8)
                   title-scene)))))

;; Ss KeyEvent -> Ss or Rs
;; changes difficulty if the player pushes the left of right arrow key
;; creates a Runstate once the shift key is pushed

(check-expect (change-difficulty 'easy "right") 'medium)
(check-expect (change-difficulty 'medium "left") 'easy)
(check-expect (change-difficulty 'hard "right") 'easy)
(check-expect (rs? (change-difficulty 'hard "down")) true)
(check-expect (change-difficulty 'easy "up") 'easy)

(define (change-difficulty s k)
  (cond
    [(key=? k "right") (move-difficulty-right s)]
    [(key=? k "left") (move-difficulty-left s)]
    [(key=? k "down") (make-start-rs s)]
    [else s]))

;; Ss -> Rs
;; makes the start game Rs

(check-expect (rs? (make-start-rs 'easy)) true)

(define (make-start-rs diff)
  (make-rs (standby-to-active (make-standby diff))
           (list '() '() '() '() '() '())
           (make-standby diff)
           0
           diff))

;; Ss -> Ss
;; moves the difficulty in response to the right key

(check-expect (move-difficulty-right 'easy) 'medium)
(check-expect (move-difficulty-right 'medium) 'hard)
(check-expect (move-difficulty-right 'hard) 'easy)

(define (move-difficulty-right s)
  (cond
    [(symbol=? s 'easy) 'medium]
    [(symbol=? s 'medium) 'hard]
    [(symbol=? s 'hard) 'easy]))

;; Ss -> Ss
;; moves the difficulty in response to the left key

(check-expect (move-difficulty-left 'easy) 'hard)
(check-expect (move-difficulty-left 'medium) 'easy)
(check-expect (move-difficulty-left 'hard) 'medium)

(define (move-difficulty-left s)
  (cond
    [(symbol=? s 'easy) 'hard]
    [(symbol=? s 'medium) 'easy]
    [(symbol=? s 'hard) 'medium]))

;; =================================================================================================
;; Pausing the Game

(define ps1 (make-ps empty empty empty 1 'easy))

;; Ps -> Image
;; renders the ps

(check-expect (image? (render-pause ps1)) true)

(define (render-pause ps)
  (overlay (text "PAUSED" 100 "black")
           (render-rs (make-rs (ps-active ps)
                               (ps-placed ps)
                               (ps-standby ps)
                               (ps-score ps)
                               (ps-diff ps)))))

;; Ps KeyEvent -> Rs or Ps
;; unpauses the game if the p key is pressed

(check-expect (unpause ps1 "d") ps1)
(check-expect (unpause ps1 "p") (make-rs empty empty empty 1 'easy))

(define (unpause p k)
  (if (key=? k "p")
      (make-rs (ps-active p) (ps-placed p) (ps-standby p) (ps-score p) (ps-diff p))
      p))

;; =================================================================================================
;; The Actual Game

;; REPOSTED FOR POSTERITY
;; (define-struct rs (active placed standby score diff))
;; Rs [Runstate] is a struct:
;; (make-rs [List-of Hat] [List-of [List-of Hat]] [List-of Number] Number Symbol])
;; interp: an rs represents:
;; - the current two hats the player is controlling
;; - the hats that are resting at the bottom of the game area, organized by 6 columns
;; - two numbers that represent the hats on standby
;; - the player's score

;l (define-struct hat (type column y))
;; A Hat is a struct:
;; (make-hat (Number Number Number))
;; interp: a hat represents
;; - the type of hat represented by a number between 0 and 4 inclusive
;; - which column in the scene it is (odd number between 0 and 12)
;; - the y coordinate of the hat

(define p-i place-image)

;; DATA EXAMPLES

(define hat-ex (make-hat 0 2 200))
(define active-ex (list (make-hat 0 3 100) (make-hat 2 5 100)))
(define column-ex (list (make-hat 2 3 373) (make-hat 0 3 403)))
(define empty-placed (list empty empty empty empty empty empty))
(define placed-ex (list (list (make-hat 1 1 413)) 
                        column-ex 
                        empty
                        empty
                        (list (make-hat 2 9 400))
                        empty))
(define five-stacked-ex (list empty (list hat-ex hat-ex hat-ex hat-ex hat-ex) empty empty empty empty))

(define empty-rs (make-rs empty empty empty 0 'easy))
(define rs-start-ex (make-rs (list (make-hat 0 7 300) (make-hat 2 9 300))
                             (list empty empty empty empty empty empty)
                             (list 1 0)
                             100 
                             'easy))

;;==================================================================================================
;; Rendering

;; render-rs
;; Rs -> Image
;; renders the current Rs as an image

(check-expect (image? (render-rs rs-start-ex)) true)

(define (render-rs r)
  (p-i (render-game-area (rs-active r) (rs-placed r)) (size 7) (size 9)
       (p-i (render-standby (rs-standby r)) (size 10) (size 1.5)
            (p-i (render-score (rs-score r)) (size 17) (size 9) ES))))

;; render-game-area
;; [List-of Hat] [List-of [List-of Hat]] -> Image
;; renders the active hats being currently controlled and the hats resting at the botoom on GAME-AREA

(check-expect (render-game-area empty empty) GAME-AREA)
(check-expect (render-game-area active-ex placed-ex)
              (render-active active-ex (render-placed placed-ex GAME-AREA)))

(define (render-game-area active bottom)
  (render-active active (render-placed bottom GAME-AREA)))

;; render-active
;; [List-of Hat] Image -> Image
;; renders the two active hats on an image i

(check-expect (render-active active-ex GAME-AREA)
              (p-i HAT0 (size 3) 100 (p-i HAT2 (size 5) 100 GAME-AREA)))

(define (render-active loh i)
  (foldr (lambda (a-hat canvas) 
           (p-i (draw-hat (hat-type a-hat)) (size (hat-column a-hat)) (hat-y a-hat) canvas)) i loh))

;; render-placed
;; [List-of [List-of Hat]] Image -> Image
;; renders the hats resting at the bottom on an image i

(check-expect (render-placed placed-ex GAME-AREA)
              (p-i HAT1 (size 1) 413
                   (p-i HAT2 (size 3) 373
                        (p-i HAT0 (size 3) 403
                             (p-i HAT2 (size 9) 400 GAME-AREA)))))

(define (render-placed bottom i)
  (foldr (lambda (column canvas) (render-column column canvas)) GAME-AREA bottom))

;; render-column
;; [List-of Hat] Image -> Image
;; places the hats at the approriate location on an image i

(check-expect (render-column column-ex GAME-AREA)
              (p-i HAT2 (size 3) 373
                   (p-i HAT0 (size 3) 403 GAME-AREA)))

(define (render-column loh i) 
  (foldr (lambda (current canvas) 
           (p-i (draw-hat (hat-type current)) (size (hat-column current)) (hat-y current) canvas)) i loh))

;; render-standby
;; [List-of Number] -> Image
;; renders the hats on standy on STANDBY-AREA
;; CONSTRAINT: there will ALWAYS be two numbers representing hats in the list

(check-expect (render-standby empty) STANDBY-AREA)
(check-expect (render-standby (list 0 2))
              (p-i HAT0 (size 1) 30
                   (p-i HAT2 (size 3) 30 STANDBY-AREA)))

(define (render-standby loh)
  (local ((define mid (/ (image-height STANDBY-AREA) 2)))
    (if (empty? loh)
        STANDBY-AREA
        (p-i (draw-hat (first loh)) (size 1) mid
             (p-i (draw-hat (second loh)) (size 3) mid STANDBY-AREA)))))

;; render-score
;; Number -> Image
;; renders the score on SCORE-AREA

(check-expect (render-score 100) (overlay (text "100" 60 "black") SCORE-AREA))

(define (render-score n)
  (overlay (text (number->string n) 60 "black")
           SCORE-AREA))

;; draw-hat
;; Number -> Image
;; draws the approratie hat for the input number 0 <= n <= 4

(check-expect (image? (draw-hat 0)) true)
(check-expect (image? (draw-hat 1)) true)
(check-expect (image? (draw-hat 2)) true)
(check-expect (image? (draw-hat 3)) true)
(check-expect (image? (draw-hat 4)) true)

(define (draw-hat n)
  (cond
    [(= n 0) HAT0]
    [(= n 1) HAT1]
    [(= n 2) HAT2]
    [(= n 3) HAT3]
    [(= n 4) HAT4]))

;; =================================================================================================
;; UPDATE

;; update-gs
;; Rs -> Rs
;; consolidates all the helper functins into one function for updating a Gs

(check-expect (rs? (update-rs rs-start-ex)) true)

(define (update-rs rs)
  (local ((define new-active (hats-fall-down (rs-active rs) (rs-diff rs))))
    (make-rs (make-new-active? (rs-standby rs) new-active (rs-placed rs))
             (remove-five-stacked (auto-drop-active new-active (rs-placed rs)))
             (make-new-standby? (rs-standby rs) new-active (rs-placed rs) (rs-diff rs))
             (update-score (auto-drop-active new-active (rs-placed rs)) (rs-score rs))
             (rs-diff rs)))) 

;; make-standby
;; Symbol -> [List-of Number]
;; makes a new set of hats on standby

(check-expect (length (make-standby 'easy)) 2)
(check-expect (length (make-standby 'medium)) 2)
(check-expect (length (make-standby 'hard)) 2)

(define (make-standby difficulty)
  (cond
    [(eq? difficulty 'easy) (list (random 3) (random 3))]
    [(eq? difficulty 'medium) (list (random 4) (random 4))]
    [(eq? difficulty 'hard) (list (random 5) (random 5))]))

;; make-new-standby? 
;; [List-of Number] [List-of Hat] [List-of [List-of Hat]] Symbol -> [List-of Number]
;; makes a new standby if hats get auto placed

(define (make-new-standby? standby active placed diff)
  (if  (can-replace? active placed)
       (make-standby diff)
       standby))

;; standby-to-active
;; [List-of Number] -> [List-of Hat]
;; convets the hats currently on standby to active hats at the top of the game area

(check-expect (standby-to-active (list 0 2)) 
              (list (make-hat 0 5 40) (make-hat 2 7 40)))

(define (standby-to-active standby)
  (list (make-hat (first standby) 5 40)
        (make-hat (second standby) 7 40)))

;; five-stacked?
;; [List-of Hat] -> Boolean
;; are the first 5 hats on the list the same type?

(check-expect (five-stacked? empty) false)
(check-expect (five-stacked? (list hat-ex)) false)
(check-expect (five-stacked? (list hat-ex hat-ex hat-ex hat-ex hat-ex)) true)
(check-expect (five-stacked? (list hat-ex hat-ex hat-ex hat-ex (make-hat 2 2 200))) false)

(define (five-stacked? column)
  (local ((define (type x) (hat-type x)))
  (cond
    [(empty? column) false]
    [(> 5 (length column)) false]
    [else (and (= (type (first column)) (type (second column)))
               (= (type (first column)) (type (third column)))
               (= (type (first column)) (type (fourth column)))
               (= (type (first column)) (type (fifth column))))])))

;; remove-five-stacked
;; [List-of [List-of Hat]] -> [List-of [List-of Hat]]
;; removes all 5-stacked hats from the placed field

(check-expect (remove-five-stacked placed-ex) placed-ex)
(check-expect (remove-five-stacked five-stacked-ex)
              (list empty empty empty empty empty empty))

(define (remove-five-stacked placed)
  (local ((define (remove-five x) (rest (rest (rest (rest (rest x)))))))
    (map (λ (column) (if (five-stacked? column) (remove-five column) column)) placed)))

;; auto-drop-active
;; [List-of Hat] [List-of [List-of Hat]] -> [List-of [List-of Hat]]
;; auto drops the active if they get too low

(check-expect (auto-drop-active active-ex empty-placed) empty-placed)
(check-expect (auto-drop-active 
               (list (make-hat 0 5 400) (make-hat 0 7 400)) placed-ex)
              (drop-hats (list (make-hat 0 5 400) (make-hat 0 7 400)) placed-ex))
  
(define (auto-drop-active active placed)
  (if (can-replace? active placed)
      (drop-hats active placed)
      placed))

;; make-new-active?
;; [List-of Number] [List-of Hat] [List-of [List-of Hat]] -> [List-of Hat]
;; makes a new active if the active gets auto placed

(check-expect (make-new-active? (list 0 2) active-ex empty-placed) active-ex)
(check-expect (make-new-active? (list 0 2) (list (make-hat 0 9 390) (make-hat 0 7 390)) placed-ex)
              (standby-to-active (list 0 2)))

(define (make-new-active? standby active placed)
  (if (can-replace? active placed)
      (standby-to-active standby)
      active))

;; can-replace?
;; [List-of Hat] [List-of [List-of Hat]] -> Boolean
;; checks whether or not both hats can be auto placed

(check-expect (can-replace? active-ex empty-placed) false)

(define (can-replace? active placed)
  (ormap (λ (hat) (can-place? hat placed)) active))

;; can-place?
;; Hat [List-of [List-of Hat]] -> Boolean
;; checks whether the hat can be auto-placed by the software

(check-expect (can-place? (make-hat 0 3 400) empty-placed) true)
(check-expect (can-place? (make-hat 0 3 200) empty-placed) false)
(check-expect (can-place? (make-hat 2 9 390) placed-ex) true)

(define (can-place? hat placed)
  (local ((define type (draw-hat (hat-type hat)))
          (define y (hat-y hat))
          (define image-center (/ (image-height type) 2)))
    (ormap (λ (column) (cond
                         [(empty? column) (>= (+ image-center y) 408)]
                         [else (>= image-center (- (hat-y (first column)) y))]))
           placed)))

;; hats-fall-down
;; [List-of Hat] Symbol -> [List-of Hat]
;; moves the active hats down every clock tick. Hats move faster on hard difficulty

(check-expect (hats-fall-down (list (make-hat 0 5 40) (make-hat 2 7 40)) 'easy)
              (list (make-hat 0 5 42) (make-hat 2 7 42)))
(check-expect (hats-fall-down (list (make-hat 0 5 40) (make-hat 2 7 40)) 'medium)
              (list (make-hat 0 5 42) (make-hat 2 7 42)))
(check-expect (hats-fall-down (list (make-hat 0 5 40) (make-hat 2 7 40)) 'hard)
              (list (make-hat 0 5 43) (make-hat 2 7 43)))

(define (hats-fall-down loh difficulty)
  (cond
    [(or (eq? difficulty 'easy) (eq? difficulty 'medium))
     (foldr (lambda (h base) 
              (cons (make-hat (hat-type h) (hat-column h) (+ (hat-y h) 2)) base)) 
            empty loh)]
    [(eq? difficulty 'hard)
     (foldr (lambda (h base) 
              (cons (make-hat (hat-type h) (hat-column h) (+ (hat-y h) 3)) base)) 
            empty loh)]))

;; update-score
;; [List-of [List-of Hat]] Number -> Number
;; adds 100 to the score every time hats get 5 stacked

(check-expect (update-score placed-ex 100) 100)

(define (update-score placed score)
  (foldr (lambda (column score) (if (five-stacked? column) (+ 100 score) score)) score placed))
  


;; =================================================================================================
; KEY-HANDLER

;; DATA EXMAPLES FOR TESTING PURPOSES

(define placed0 (list empty empty empty empty empty empty))
(define placed1 (list empty empty (list (make-hat 0 5 404)) empty empty empty))
(define active1 (make-hat 3 5 200))
(define active2 (make-hat 4 9 200))
(define active3 (list active1 active2))

;; rs-keyh
;; Rs KeyEvent -> Rs
;; main key-handler function that hands tasks off
;; to various helper functions depending on the input key

(check-expect (rs-keyh rs-start-ex  "left")
              (apply-to-rs rs-start-ex move-active-left))
(check-expect (rs-keyh rs-start-ex "right")
              (apply-to-rs rs-start-ex move-active-right))
(check-expect (rs-keyh rs-start-ex " ")
              (apply-to-rs rs-start-ex switch-active))
(check-expect (rs? (rs-keyh rs-start-ex "down")) true)
(check-expect (ps? (rs-keyh rs-start-ex "p")) true)
(check-expect (rs-keyh rs-start-ex "up") rs-start-ex)

(define (rs-keyh rs k)
  (cond
    [(key=? k "left") (apply-to-rs rs move-active-left)]
    [(key=? k "right") (apply-to-rs rs move-active-right)]
    [(key=? k " ") (apply-to-rs rs switch-active)] 
    [(key=? k "down") (new-rs-after-drop rs)]
    [(key=? k "p") (pause-game rs)]
    [else rs]))

;; pause-game
;; Rs -> Ps
;; pauses the game

(check-expect (ps? (pause-game rs-start-ex)) true)

(define (pause-game rs)
  (make-ps (rs-active rs)
           (rs-placed rs)
           (rs-standby rs)
           (rs-score rs)
           (rs-diff rs)))

;; apply-to-rs
;; Rs Function -> Rs
;; applies some function f to the active field of the rs
;; retains the rest of the rs

(check-expect (apply-to-rs rs-start-ex move-active-left)
              (make-rs (list (make-hat 0 5 300) (make-hat 2 7 300))
                       (list empty empty empty empty empty empty)
                       (list 1 0)
                       100
                       'easy))
(check-expect (apply-to-rs rs-start-ex move-active-right)
              (make-rs (list (make-hat 0 9 300) (make-hat 2 11 300))
                       (list empty empty empty empty empty empty)
                       (list 1 0)
                       100
                       'easy))

(define (apply-to-rs rs f)
  (make-rs (f (rs-active rs))
           (rs-placed rs)
           (rs-standby rs)
           (rs-score rs)
           (rs-diff rs)))

;; new-rs-after-drop
;; Rs -> Rs
;; constructs a new rs after dropping the hats

(check-expect (rs? (new-rs-after-drop (make-rs active3 placed1 (list 2 2) 100 'easy))) true)

(define (new-rs-after-drop rs)
  (make-rs (standby-to-active (rs-standby rs))
           (drop-hats (rs-active rs) (rs-placed rs))
           (make-standby (rs-diff rs))
           (rs-score rs)
           (rs-diff rs)))

;; drop-hats
;; [List-of Hat] [List-of [List-of Hat]] -> [List-of [List-of Hat]]
;; drops the active hats the the bottom 

(check-expect (drop-hats active3 placed1)
              (list empty
                    empty 
                    (list (make-hat 3 5 375) (make-hat 0 5 404))
                    empty 
                    (list (make-hat 4 9 408))
                    empty))

(define (drop-hats active placed)
  (cond
    [(empty? (rest active)) (place-hat-in-column (first active) placed)]
    [else (drop-hats (rest active) (place-hat-in-column (first active) placed))]))
  
;; place-hat-in-column
;; Hat [List-of [List-of Hat]] -> [List-of [List-of Hat]]
;; chooses corrent column in the list to place the hat in

(define (place-hat-in-column hat placed)
    (list (place-hat-here? 1 hat (first placed))
          (place-hat-here? 3 hat (second placed))
          (place-hat-here? 5 hat (third placed))
          (place-hat-here? 7 hat (fourth placed))
          (place-hat-here? 9 hat (fifth placed))
          (place-hat-here? 11 hat (sixth placed))))
          
;; place-hat-here?
;; Hat [List-of Hat] Number -> [List-of Hat]
;; determins the right column to place the hat in    

(check-expect (place-hat-here? 1 (make-hat 0 3 200) empty) empty)
(check-expect (place-hat-here? 1 (make-hat 0 1 200) empty) (list (make-hat 0 1 404)))
(check-expect (place-hat-here? 1 (make-hat 0 1 200) (list (make-hat 0 1 404)))
              (list (make-hat 0 1 399) (make-hat 0 1 404)))

(define (place-hat-here? n hat placed)
  (cond
    [(not (eq? (hat-column hat) n)) placed]
    [else (cond
            [(empty? placed) (place-hat hat empty)]
            [else (place-hat hat placed)])]))

;; place-hat
;; Hat [List-of Hat] -> [List-of Hat]
;; selects the appropriate helper function to place the hat
;; depending on what type of hat is below it
;; CONSTRAINT: h must be in the same column as placed

(check-expect (place-hat active1 empty) (list (make-hat 3 5 410)))
(check-expect (place-hat active2 empty) (list (make-hat 4 9 408)))
(check-expect (place-hat active1 (list (make-hat 0 5 410)))  
              (list (make-hat 3 5 (place-0 active1 410)) (make-hat 0 5 410)))
(check-expect (place-hat active1 (list (make-hat 1 5 410))) 
              (list (make-hat 3 5 (place-1 active1 410)) (make-hat 1 5 410)))
(check-expect (place-hat active1 (list (make-hat 2 5 410))) 
              (list (make-hat 3 5 (place-2 active1 410)) (make-hat 2 5 410)))
(check-expect (place-hat active1 (list (make-hat 3 5 410))) 
              (list (make-hat 3 5 (place-3 active1 410)) (make-hat 3 5 410)))
(check-expect (place-hat active1 (list (make-hat 4 5 410))) 
              (list (make-hat 3 5 (place-4 active1 410)) (make-hat 4 5 410)))
 
(define (place-hat h placed)
  (local ((define (type=? h t) (eq? (hat-type h) t))
          (define type (hat-type h))
          (define column (hat-column h))
          (define y (cond [(empty? placed) 0] [else (hat-y (first placed))]))
          (define (place-x x) (make-hat type column (x h y))))
    (cond
      [(empty? placed) (list (make-hat type column (place-empty h)))]
      [(type=? (first placed) 0) (cons (place-x place-0) placed)]
      [(type=? (first placed) 1) (cons (place-x place-1) placed)]
      [(type=? (first placed) 2) (cons (place-x place-2) placed)]
      [(type=? (first placed) 3) (cons (place-x place-3) placed)]
      [(type=? (first placed) 4) (cons (place-x place-4) placed)])))

;; place-empty
;; Hat -> Number
;; determines height to place the hat in an empty column depending on hat type

(check-expect (place-empty (make-hat 0 3 100)) 404)
(check-expect (place-empty (make-hat 1 3 100)) 408)
(check-expect (place-empty (make-hat 2 3 100)) 400)
(check-expect (place-empty (make-hat 3 3 100)) 410)
(check-expect (place-empty (make-hat 4 3 100)) 408)

(define (place-empty h)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) 404]
    [(type? 1) 408]
    [(type? 2) 400]
    [(type? 3) 410]
    [(type? 4) 408])))  

;; place-0
;; Hat Number -> Number
;; determines height to place the hat on top of the y coord of a HAT0

(check-expect (place-0 (make-hat 0 3 100) 400) 395)
(check-expect (place-0 (make-hat 1 3 100) 400) 376)
(check-expect (place-0 (make-hat 2 3 100) 400) 369)
(check-expect (place-0 (make-hat 3 3 100) 400) 371)
(check-expect (place-0 (make-hat 4 3 100) 400) 373)

(define (place-0 h y)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) (- y 5)]
    [(type? 1) (- y 24)]
    [(type? 2) (- y 31)]
    [(type? 3) (- y 29)]
    [(type? 4) (- y 27)])))                

;; place-1
;; Hat Number -> Number
;; determines height to place the hat on top of the y coord of a HAT1

(check-expect (place-1 (make-hat 0 3 100) 400) 369)
(check-expect (place-1 (make-hat 1 3 100) 400) 377)
(check-expect (place-1 (make-hat 2 3 100) 400) 367)
(check-expect (place-1 (make-hat 3 3 100) 400) 376)
(check-expect (place-1 (make-hat 4 3 100) 400) 373)

(define (place-1 h y)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) (- y 31)]
    [(type? 1) (- y 23)]
    [(type? 2) (- y 33)]
    [(type? 3) (- y 24)]
    [(type? 4) (- y 27)]))) 

;; place-2
;; Hat Number -> Number
;; determines height to place the hat on top of the y coord of a HAT2

(check-expect (place-2 (make-hat 0 3 100) 400) 393)
(check-expect (place-2 (make-hat 1 3 100) 400) 390)
(check-expect (place-2 (make-hat 2 3 100) 400) 394)
(check-expect (place-2 (make-hat 3 3 100) 400) 384)
(check-expect (place-2 (make-hat 4 3 100) 400) 385)

(define (place-2 h y)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) (- y 7)]
    [(type? 1) (- y 10)]
    [(type? 2) (- y 6)]
    [(type? 3) (- y 16)]
    [(type? 4) (- y 15)]))) 

;; place-3
;; Hat Number -> Number
;; determines height to place the hat on top of the y coord of a HAT3

(check-expect (place-3 (make-hat 0 3 100) 400) 383)
(check-expect (place-3 (make-hat 1 3 100) 400) 384)
(check-expect (place-3 (make-hat 2 3 100) 400) 379)
(check-expect (place-3 (make-hat 3 3 100) 400) 389)
(check-expect (place-3 (make-hat 4 3 100) 400) 385)

(define (place-3 h y)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) (- y 17)]
    [(type? 1) (- y 16)]
    [(type? 2) (- y 21)]
    [(type? 3) (- y 11)]
    [(type? 4) (- y 15)])))

;; place-4
;; Hat Number -> Number
;; determines height to place the hat on top of the y coord of a HAT4

(check-expect (place-4 (make-hat 0 3 100) 400) 374)
(check-expect (place-4 (make-hat 1 3 100) 400) 386)
(check-expect (place-4 (make-hat 2 3 100) 400) 376)
(check-expect (place-4 (make-hat 3 3 100) 400) 382)
(check-expect (place-4 (make-hat 4 3 100) 400) 387)

(define (place-4 h y)
  (local ((define (type? t) (eq? (hat-type h) t)))
  (cond
    [(type? 0) (- y 26)]
    [(type? 1) (- y 14)]
    [(type? 2) (- y 24)]
    [(type? 3) (- y 18)]
    [(type? 4) (- y 13)])))

;; move-hats, move-active-left, move-active-right
;; [List-of Hat] [Number Number -> Number] -> [List-of Hat]
;; moves the active hats to either the left or the right (- or +)

(check-expect (move-active-left (list (make-hat 0 3 200) (make-hat 2 5 200)))
              (list (make-hat 0 1 200) (make-hat 2 3 200)))
(check-expect (move-active-right (list (make-hat 0 3 200) (make-hat 2 5 200)))
              (list (make-hat 0 5 200) (make-hat 2 7 200)))
(check-expect (move-active-left (list (make-hat 0 1 200) (make-hat 2 3 200)))
              (list (make-hat 0 1 200) (make-hat 2 3 200)))
(check-expect (move-active-right (list (make-hat 0 9 200) (make-hat 2 11 200)))
              (list (make-hat 0 9 200) (make-hat 2 11 200)))

;; move-active-left
(define (move-active-left active-hats)
  (move-active active-hats -))

;; move-active-right
(define (move-active-right active-hats)
  (move-active active-hats +))

(define (move-active active-hats f)
  (local ((define hatcol1 (hat-column (first active-hats)))   ;; column of 1st hat
          (define hatcol2 (hat-column (second active-hats)))) ;; column of 2nd hat
  (cond
    [(and (eq? f -) (or (= hatcol1 1) (= hatcol2 1))) active-hats]
    [(and (eq? f +) (or (= hatcol1 11) (= hatcol2 11))) active-hats]
    [else (foldr (lambda (h base) 
                   (cons (make-hat (hat-type h) (f (hat-column h) 2) (hat-y h)) base)) 
                 empty active-hats)])))

;; switch-hats
;; [List-of Hat] -> [List-of Hat]
;; switches the order of the two active hats in the list

(check-expect (switch-active (list (make-hat 0 3 200) (make-hat 2 5 200)))
              (list (make-hat 0 5 200) (make-hat 2 3 200)))

(define (switch-active active)
  (list (make-hat (hat-type (first active)) (hat-column (second active)) (hat-y (first active)))
        (make-hat (hat-type (second active)) (hat-column (first active)) (hat-y (second active)))))

;; =================================================================================================
;; Stop-Handler

;; stop-the-game?
;; Rs -> Boolean
;; stop handler function for rs

(check-expect (stop-the-game? empty-rs) false)

(define (stop-the-game? rs)
  (hats-too-high? (rs-placed rs)))

;; hats-too-high?
;; [List-of [List-of Hat]] -> Boolean
;; is any one column stacked too high?

(check-expect (hats-too-high? placed-ex) false)
(check-expect (hats-too-high? (list (list hat-ex hat-ex hat-ex hat-ex hat-ex hat-ex hat-ex hat-ex hat-ex hat-ex)
                              empty empty empty empty empty))
              true)            

(define (hats-too-high? placed)
  (ormap (λ (column) (>= (length column) 10)) placed))

;; end-game-scene
;; Rs -> Image
;; renders the game-over scene

(check-expect (image? (end-game-scene rs-start-ex)) true)

(define (end-game-scene rs)
  (overlay (text "GAME OVER" 100 "black")
           (render-rs rs)))







