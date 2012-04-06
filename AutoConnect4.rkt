#lang class/3
(require class/universe)
(require 2htdp/image)

;; Constants
(define PIECE-SIZE/PIXELS 25)
(define BOARD-WIDTH/PIECES 7)
(define BOARD-HEIGHT/PIECES 6)
(define TO-WIN 4)

(define EMPTY-GRID
  (build-list BOARD-WIDTH/PIECES
              (lambda(a)
                (build-list BOARD-HEIGHT/PIECES
                            (lambda(b) 'mt)))))
(define EMPTY-SCENE (empty-scene (* PIECE-SIZE/PIXELS BOARD-WIDTH/PIECES)
                                 (* PIECE-SIZE/PIXELS BOARD-HEIGHT/PIECES)))


;; A Request is one of:
;;  - 'assignment
;;  - 'grid
;;  - 'game-over
;;  - 'game-on

;; A Player is one of:
;;  - 'p1
;;  - 'p2

;; A Piece is one of:
;;  - 'p1
;;  - 'p2
;;  - 'mt

;; A Data is a (list Grid Player)

;; A Game-Data is a (list Request Data)

;; A Game-State is one of:
;;  - 'p1
;;  - 'p2
;;  - 'tie
;;  - 'game-on

;====================
; Helper Functions ||
;===============================================================================

;; replace-first-match : Any Any List -> List
;; Replaces the first item in the list that matches find.
(define (replace-first find replacement lst)
  (if (equal? find (first lst))
      (cons replacement (rest lst))
      (cons (first lst) (replace-first
                         find
                         replacement
                         (rest lst)))))

(check-expect
 (replace-first 'find-me 'replaced (list 'a 'b 'c 'find-me 'find-me 'd 'e))
 (list  'a 'b 'c 'replaced 'find-me 'd 'e))
(check-expect (replace-first 3 "replaced" (list 1 1 2 1 3 2 3 4 5 5 6 7))
              (list 1 1 2 1 "replaced" 2 3 4 5 5 6 7))

;; pick-random : [Listof X] -> X
;; produces a random element of the list
(define (pick-random lst)
  (list-ref lst (random (length lst))))

;========
; Grid ||
;===============================================================================
;; A Grid is a (new grid% [Listof [Listof Piece]])

(define-class grid%
  (fields grid)
  
  ;; walk : Number Number Number Number -> [ListOf Piece]
  ;; Generates a list of pieces from the starting positon,
  ;; walking until the edge of the board in the direction
  ;; of the step.
  (define (walk start-x start-y step-x step-y)
    (cond [(and (< -1 start-x BOARD-WIDTH/PIECES)
                (< -1 start-y BOARD-HEIGHT/PIECES))
           (cons (get-piece-at start-x start-y)
                 (walk (+ start-x step-x)
                       (+ start-y step-y)
                       step-x
                       step-y))]
          [else empty]))
  
  ;; x-in-a-row? : Piece Number [ListOf Piece] -> Boolean
  ;; Checks the list to see if the given piece can found
  ;; count times consecutively.
  (define (x-in-a-row? player count lst)
    (cond
      [(zero? count) true]
      [(empty? lst) false]
      [(and (= 1 count) (equal? player (first lst))) true]
      [(empty? (rest lst)) false]
      [(and (equal? player (first lst))
            (equal? player (first (rest lst))))
       (x-in-a-row? player (sub1 count) (rest lst))]
      [else (x-in-a-row? player TO-WIN (rest lst))]))
  
  
  ;; won? : Player -> Boolean
  ;; Checks if the given player has the number in a row needed
  ;; to win.
  (define (won? player)
    (or (ormap (lambda(y)
                 (or 
                  (x-in-a-row? player TO-WIN (walk y 
                                                   (sub1 BOARD-HEIGHT/PIECES)
                                                   1 -1))
                  (x-in-a-row? player TO-WIN (walk 0 y 1 0))
                  (x-in-a-row? player TO-WIN (walk 0 y 1 1))
                  (x-in-a-row? player TO-WIN (walk 0 y 1 -1))))
               
               (build-list BOARD-HEIGHT/PIECES (lambda(y)y)))
        (ormap (lambda(col)
                 (x-in-a-row? player TO-WIN col))
               (field grid))))
  
  ;; availability? -> Boolean
  ;; Checks if there are any open spaces on the grid
  (define (availability?)
    (ormap 
     (lambda (col)
       (ormap (lambda (spot)
                (symbol=? 'mt spot)) col)) (field grid)))
  
  ;; get-piece-at : Number Number -> Piece
  ;; Fetches the piece at the given column, row
  (define (get-piece-at col row)
    (list-ref (list-ref (field grid) col) row))
  
  ;; can-place? : Number -> Boolean
  ;; Determines if there is room to place a Player in a column
  (define (can-place? col)
    (not (false? (member 'mt (list-ref (field grid) col)))))
  
  ;; place-piece : Number Player -> Grid
  ;; Places a Player in the given column
  (define (place-piece column piece)
    (new grid%
         (build-list
          (length (field grid))
          (lambda (cur-col)
            ;;Check if this is the column we're placing in
            (if (= column cur-col) 
                ;; Find the unoccupied spot closest to the bottom. Reverses
                ;; the list first to make recursion easy. Reverse again to
                ;; restore list to normal order.
                (reverse
                 (replace-first
                  'mt piece
                  (reverse (list-ref (field grid) cur-col))))
                ;; If this is not the column, leave it alone
                (list-ref (field grid) cur-col))))))
  
  ;; draw-piece : Piece -> Image
  ;; Draws the piece
  (define (draw-piece piece)
    (overlay
     (circle PIECE-SIZE/PIXELS 'outline 'black)
     (circle PIECE-SIZE/PIXELS
             'solid
             (cond [(symbol=? 'p1 piece) 'red]
                   [(symbol=? 'p2 piece) 'black]
                   [else 'white]))))
  
  ;; draw-col : col -> image
  ;; Draws the given column (ListOf Piece)
  (define (draw-col col)
    (foldl (lambda (piece scn)
             (above scn
                    (draw-piece piece)))
           (draw-piece (first col))
           (rest col)))
  
  ;; draw : -> Image
  ;; Draws this grid%
  (define (draw)
    (foldl (lambda (column scn)
             (beside scn (draw-col column)))
           (draw-col (first (field grid)))
           (rest (field grid)))))

;==========
; Server ||
;===============================================================================
;; A Universe is a (new universe% Player|False Player|False Player Grid)

(define-class universe% 
  (fields p1 p2 turn grid)
  
  ;; on-new : IWorld -> Bundle
  ;; handles new connections
  (define (on-new iw)
    (cond [(false? (field p1))
           (make-bundle
            (new universe% iw false 'p1 (field grid))
            (list (make-mail iw (list 'assignment 'p1)))
            empty)]
          [(false? (field p2))
           (make-bundle
            (new universe% (field p1) iw (field turn) (field grid))
            (list (make-mail iw (list 'assignment 'p2))
                  (make-mail iw (list 'grid (list ((grid) . grid) (turn)
                                                  ))))
            empty)]
          [else (make-bundle this empty (list iw))]))
  
  ;; game-over? : -> Game-State
  ;; determines if the game is over
  (define (game-over?)
    (cond
      [((grid) . won? 'p1) 'p1]
      [((grid) . won? 'p2) 'p2]
      [(not ((grid) . availability?)) 'tie]
      [else 'game-on]))
  
  ;; next-turn : -> Player
  ;; returns who's turn it is
  (define (next-turn)
    (if (symbol=? (field turn) 'p1) 'p2 'p1))
  
  ;; iw->symbol : IWorld -> Player
  ;; converts from an IWorld to a Symbol
  (define (iw->symbol iw)
    (if (equal? iw (field p1)) 'p1 'p2))
  
  ;; mail-both : Game-Data -> [Listof Mail]
  ;; creates a list so both clients receive mail
  (define (mail-both data)
    (append (list (make-mail (field p1) data))
            (if (false? (p2)) empty (list (make-mail (field p2) data)))))
  
  ;; on-msg : IWorld Number -> Bundle
  ;; handles all messages from clients
  (define (on-msg iw col-choice)
    (local [(define game-state (game-over?))]
      (cond
        ;; if it's not this player's turn, ignore the request
        [(not (equal? (field turn) (iw->symbol iw))) (make-bundle this empty empty)]
        [(send (grid) can-place? col-choice)
         (local
           [(define sender (iw->symbol iw))
            (define new-grid (send (grid) place-piece col-choice sender))
            (define new-uni (new universe% (p1) (p2) (next-turn) new-grid))
            (define new-game-state (send new-uni game-over?))]
           (cond [(symbol=? new-game-state 'game-on)
                  (make-bundle
                   new-uni 
                   (mail-both (list 'grid
                                    (list (send new-grid grid) (next-turn))))
                   empty)]
                 [else (make-bundle (universe% (p1) (p2) 'p1 (grid% EMPTY-GRID))
                                    (append (mail-both (list 'game-over new-game-state))
                                            (mail-both (list 'grid (list EMPTY-GRID 'p1))))
                                    
                                    empty)]))]
        [else (make-bundle 
               (universe% (p1) (p2) 'p1 (grid% EMPTY-GRID))
               (mail-both 
                (list 'grid 
                      (list (send (grid) grid)
                            (field turn))))
               empty)]))))


;=========
; World ||
;===============================================================================
;; A World is either a:
;;  - Human-World
;;  - Auto-World

(define-interface <world%>
  [;; A Player, the player attempting a move
   player
   ;; A Game-State
   status
   ;; A Grid
   grid
   ;; A Player, who's turn is it?
   turn
   ;; to-draw : -> Scene
   ;; draws the game to a scene
   to-draw
   ;; on-receive : Game-Data -> World%
   ;; responds to messages received from the server
   on-receive
   ;; each make has a different contract, but same purpose statement
   ;; (in Human-World) make : Player Game-State Grid Player -> Human-World
   ;; (in Auto-World) make : Player Game-State Grid Player Number Number
   ;;                        Number Number -> Auto-World
   ;; makes a World of the original type (auto or human)
   make])

;===============
; Human-World ||
;===============================================================================
;; A Human-World is a (new hum-world% Player Game-State Grid Player)

(define-class hum-world%
  (fields player status grid turn)
  
  (define (register) "127.0.0.1")
  
  (define (to-draw)
    (above
     (cond [(equal? (field player) 'p1) (text "Player 1" 20 'red)]
           [(equal? (field player) 'p2) (text "Player 2" 20 'black)]
           [(false? (field player)) (text "Waiting to start..." 20 'black)])
     (text (cond 
             [(and (equal? (field status) 'game-over) 
                   (not (equal? (field player) 
                                (field turn))))
              "You won"]
             [(and (equal? (field status) 'game-over) (equal? (field player) 
                                                              (field turn)))
              "You Lost"]
             [(and (not (false? (field player)))
                   (equal? (field player) (field turn))) "Your Turn"]
             [else "Other Player's Turn"])
           16
           'black)
     ((field grid) . draw)))
  
  (define (on-receive d)
    (local [(define request (first d))
            (define data (second d))]
      (cond
        [(symbol=? request 'assignment)
         (this . make data (field status) (field grid) (field turn))]
        [(symbol=? request 'grid)
         (this . make (field player) 'game-on (new grid% (first data)) (second data))]
        [(symbol=? request 'game-over)
         (this . make (field player) 'game-over (field grid) (field turn))]
        [else this])))
  
  ;; on-mouse : Number Number Mouse-Event -> Package|World
  ;; handles mouse events
  (define (on-mouse x y m)
    (cond
      [(and (string=? m "button-down")
            (equal? (field turn) (field player)))
       (make-package this (sub1 (ceiling (/ (add1 x) 
                                            (* 2 PIECE-SIZE/PIXELS)))))]
      [else this]))
  
  (define (make player game-state grid turn)
    (new hum-world% player game-state grid turn)))

;=========
; Brain ||
;===============================================================================

;; A brain is a (brain% [ListOf knowledge%])
;; Brains hold "knowledge"
(define-class brain%
  (fields intel)
  
  ;; teach : Knowledge% ->
  ;; effect: add the knowledge% to the intel
  (define (teach k)
    (set-field! intel (cons k (intel)))))

;; A knowledge% is a (knowledge% Grid Number Symbol)
;; Stores the result of a given action.
(define-class knowledge%
  (fields grid col res)
  
  ;; match? : Grid -> Boolean
  ;; determines if the grids match
  (define (match? g)
    (equal? (grid) g)))

;==============
; Auto-World ||
;===============================================================================


;; An Auto-World is a (auto-world% Player Game-State 
;;                     Grid Player Intelligence Grid Number Number Number Number)
(define-class auto-world%
  (implements <world%>)
  (fields player status grid turn brain last-grid last-move wins losses ties)
  
  (define (register) "127.0.0.1")
  
  (define (to-draw)
    (above
     (cond [(equal? (field player) 'p1) (text "Player 1 (Auto)" 20 'red)]
           [(equal? (field player) 'p2) (text "Player 2 (Auto)" 20 'black)]
           [(false? (field player)) (text "Waiting to start..." 20 'black)])
     (text (string-append "Score: "
                          (number->string (field wins))
                          " - "
                          (number->string (field losses))
                          " - "
                          (number->string (field ties)))
           16 'black)
     (text (cond 
             [(and (equal? (field status) 'game-over) 
                   (not (equal? (field player) 
                                (field turn))))
              "You won"]
             [(and (equal? (field status) 'game-over) (equal? (field player) 
                                                              (field turn)))
              "You Lost"]
             [(and (not (false? (field player)))
                   (equal? (field player) (field turn))) "Your Turn"]
             [else "Other Player's Turn"])
           16
           'black)
     ((field grid) . draw)))
  
  (define (on-receive d)
    (local [(define request (first d))
            (define data (second d))]
      (cond
        [(symbol=? request 'assignment)
         (local
           [(define ply data)
            (define next-world
              (make
               ply (status) (grid) (turn) (last-grid) (wins) (losses) (ties)))]
           (if (equal? (turn) ply)
               (make-package next-world (send next-world pick))
               next-world))]
        [(symbol=? request 'grid)
         (local
           [(define rgrd (grid% (first data)))
            (define rtrn (second data))
            (define next-world
              (make (player) 'game-on rgrd rtrn (grid) (wins) (losses) (ties)))]
           (if (symbol=? rtrn (player))
               (make-package next-world (send next-world pick))
               next-world))]
        [(symbol=? request 'game-over)
         (local [(define winner data)
                 (define w (+ (wins) (if (equal? winner (player)) 1 0)))
                 (define l (+ (losses) (if (or (equal? winner (player))
                                               (equal? winner 'tie))
                                           0 1)))
                 (define t (+ (ties) (if (equal? winner 'tie) 1 0)))
                 (define next-world
                   (make (player) 'game-on (grid% EMPTY-GRID)
                         'p1 (last-grid) w l t))]
           (begin ((brain) . teach (knowledge% ((last-grid) . grid)
                                               (last-move) winner))
                  next-world))]
        [else this])))
  
  ;; win-col : Number Symbol -> Number
  ;; Figures out the column where the player can win. Otherwise, -1.
  (define (win-col cols ply)
    (cond [(= -1 cols) -1]
          [(not ((grid) . can-place? cols)) (win-col (sub1 cols) ply)]
          [(((grid) . place-piece cols ply) . won? ply) cols]
          [else (win-col (sub1 cols) ply)]))
  
  ;; pick : -> Number
  ;; Determine the best possible move to make
  (define (pick)
    (local [; Columns which a Player can be placed in
            (define avail-cols (filter (λ(x)(send (grid) can-place? x))
                                       (build-list  BOARD-WIDTH/PIECES
                                                    (lambda(x)x))))
            ; Every match of knowledge in my brain that is doable.
            (define matches (filter
                             (λ (x) (and (x . match? (grid))
                                         (member (x . col) avail-cols)))
                             ((brain) . intel)))
            
            ; Every win in in my knowledge
            (define poss-wins (filter (λ (x) (equal? (x . res)
                                                     (player)))
                                      matches))
            ; Every loss in my knowledge
            (define poss-loss (filter (λ (x) (false? (equal? (x . res)
                                                             (player))))
                                      matches))
            ; A computed way I can win
            (define the-win (win-col (sub1 BOARD-WIDTH/PIECES) (player)))
            ; A computed way the other player can win
            (define other-win (win-col (sub1 BOARD-WIDTH/PIECES)
                                       (if (equal? (player) 'p1) 'p2 'p1)))]
      (cond [(not (= the-win -1)) the-win]
            [(not (= other-win -1)) other-win]
            [(not (empty? poss-wins)) ((pick-random poss-wins) . col)]
            [(not (empty? poss-loss)) ((pick-random poss-loss) . col)]
            [else (if (empty? avail-cols) 0 (pick-random avail-cols))])))
  
  (define (make ply gstate grd trn last-grid w l t)
    (auto-world% ply gstate grd trn (brain) last-grid (last-move) w l t)))


;=========
; Tests ||
;===============================================================================

(define testg1 (list
                (list 'mt 'p1 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)))
(define testg2 (list
                (list 'mt 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p1 'p1 'p2 'p2 'p2)
                (list 'p1 'p2 'mt 'p2 'p2 'p2)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt 'mt 'mt)))
(define testg3 (list
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)
                (list 'mt 'mt 'mt 'p1 'p1 'p1)))
(define testg4 (list
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p1 'p1 'p1)))
(define testg5 (list
                (list 'p2 'p1 'p1 'p1 'p2 'p1)
                (list 'p1 'p2 'p2 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p2 'p1 'p2)
                (list 'p2 'p2 'p1 'p1 'p1 'p2)
                (list 'p1 'p1 'p2 'p1 'p2 'p1)
                (list 'p2 'p2 'p1 'p2 'p1 'p1)
                (list 'p2 'p2 'p1 'p2 'p1 'p1)))
(define testg6 (list
                (list 'p2 'p1 'p1 'p1 'p2 'p1)
                (list 'p1 'p2 'p2 'p1 'p1 'p1)
                (list 'p2 'p2 'p1 'p2 'p1 'p2)
                (list 'p2 'p2 'p1 'p1 'p1 'p2)
                (list 'p1 'p1 'p2 'p1 'p2 'p2)
                (list 'p2 'p2 'p1 'p2 'p1 'p2)
                (list 'p2 'p2 'p1 'p2 'p1 'p1)))

;; tests for won?
(check-expect ((new grid% testg1) . won? 'p1) true)
(check-expect ((new grid% testg1) . won? 'p2) false)
(check-expect ((new grid% testg2) . won? 'p1) true)
(check-expect ((new grid% testg2) . won? 'p2) false)
(check-expect ((new grid% testg3) . won? 'p1) true)
(check-expect ((new grid% testg3) . won? 'p2) false)
;; tests for walk and x-in-a-row? are tested by won?'s tests

;; tests for availability?
(check-expect ((grid% testg1) . availability?) true)
(check-expect ((grid% testg2) . availability?) true)
(check-expect ((grid% testg4) . availability?) false)

;; tests for get-piece-at
(check-expect ((new grid% testg1) . get-piece-at 0 0) 'mt)
(check-expect ((new grid% testg1) . get-piece-at 1 1) 'p2)
(check-expect ((new grid% testg1) . get-piece-at 1 2) 'p1)
(check-expect ((new grid% testg2) . get-piece-at 2 3) 'p2)
(check-expect ((new grid% testg2) . get-piece-at 2 0) 'p1)
(check-expect ((new grid% testg2) . get-piece-at 3 2) 'mt)

;; tests for can-place? 
(check-expect ((new grid% testg1) . can-place? 0) true)
(check-expect ((new grid% testg1) . can-place? 1) false)
(check-expect ((new grid% testg1) . can-place? 2) true)
(check-expect ((new grid% testg2) . can-place? 1) false)
(check-expect ((new grid% testg2) . can-place? 0) true)
(check-expect ((new grid% testg3) . can-place? 3) true)

;; tests for place-piece
(check-expect ((grid% testg1) . place-piece 0 'p1)
              (grid% (list
                      (list 'p1 'p1 'p1 'p1 'p1 'p1)
                      (list 'p2 'p2 'p1 'p1 'p1 'p1)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt))))
(check-expect ((grid% testg1) . place-piece 3 'p2)
              (grid% (list
                      (list 'mt 'p1 'p1 'p1 'p1 'p1)
                      (list 'p2 'p2 'p1 'p1 'p1 'p1)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'p2)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt))))
(check-expect ((grid% testg1) . place-piece 6 'p1)
              (grid% (list
                      (list 'mt 'p1 'p1 'p1 'p1 'p1)
                      (list 'p2 'p2 'p1 'p1 'p1 'p1)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'mt)
                      (list 'mt 'mt 'mt 'mt 'mt 'p1))))

;; tests for draw-piece
(check-expect ((grid% testg1) . draw-piece 'p1)
              (overlay (circle PIECE-SIZE/PIXELS 'outline 'black)
                       (circle PIECE-SIZE/PIXELS
                               'solid 'red)))
(check-expect ((grid% testg1) . draw-piece 'p2)
              (overlay (circle PIECE-SIZE/PIXELS 'outline 'black)
                       (circle PIECE-SIZE/PIXELS
                               'solid 'black)))
(check-expect ((grid% testg1) . draw-piece 'mt)
              (overlay (circle PIECE-SIZE/PIXELS 'outline 'black)
                       (circle PIECE-SIZE/PIXELS
                               'solid 'white)))

(define aw1 (auto-world% 'p1 'waiting (grid% EMPTY-GRID)
                         'p1 (brain% empty) (grid% EMPTY-GRID) 0 0 0 0))
(define aw2 (auto-world% 'p2 'waiting (grid% EMPTY-GRID)
                         'P1 (brain% empty) (grid% EMPTY-GRID) 0 0 0 0))
(define aw3 (auto-world% 'p1 'waiting (grid% testg1)
                         'p1 (brain% empty) (grid% EMPTY-GRID) 0 0 0 0))
(define hw1 (hum-world% 'p1 'waiting (grid% EMPTY-GRID) 'p1))
(define hw2 (hum-world% 'p1 'waiting (grid% EMPTY-GRID) 'p2))
(define hw3 (hum-world% 'p2 'waiting (grid% EMPTY-GRID) 'p2))

(define uni1 (universe% false false 'p1 (new grid% EMPTY-GRID)))
(define uni2 (universe% false false 'p1 (new grid% testg4)))
(define uni3 (universe% false false 'p1 (new grid% testg6)))
(define uni4 (universe% false false 'p1 (new grid% testg5)))
(define uni5 (universe% hw1 hw2 'p1 (grid% EMPTY-GRID)))
(define uni6 (universe% hw3 hw2 'p2 (grid% EMPTY-GRID)))
(define uni7 (universe% iworld1 false 'p1 (grid% EMPTY-GRID)))
(define uni8 (universe% iworld2 iworld1 'p1 (grid% EMPTY-GRID)))

;; tests for game-over?
(check-expect (uni1 . game-over?) 'game-on)
(check-expect (uni2 . game-over?) 'p1)
(check-expect (uni3 . game-over?) 'p2)
(check-expect (uni4 . game-over?) 'tie)

;; tests for next-turn
(check-expect (uni1 . next-turn) 'p2)
(check-expect (uni6 . next-turn) 'p1)

(define k1 (knowledge% (grid% testg1) 0 'p1))

;; tests for match?
(check-expect (k1 . match? (grid% testg1)) true)
(check-expect (k1 . match? (grid% testg2)) false)

;; tests for win-col
(check-expect (aw1 . win-col 5 'p1) -1)
(check-expect (aw3 . win-col 5 'p1) 5)

;; tests for to-draw
(check-expect (hw1 . to-draw) (above
                               (text "Player 1" 20 'red)
                               (text "Your Turn" 16 'black)
                               ((hw1 . grid) . draw)))
(check-expect (hw3 . to-draw) (above
                               (text "Player 2" 20 'black)
                               (text "Your Turn" 16 'black)
                               ((hw2 . grid) . draw)))
(check-expect (aw1 . to-draw) (above
                               (text "Player 1 (Auto)" 20 'red)
                               (text "Score: 0 - 0 - 0" 16 'black)
                               (text "Your Turn" 16 'black)
                               ((aw1 . grid) . draw)))
(check-expect (aw2 . to-draw) (above
                               (text "Player 2 (Auto)" 20 'black)
                               (text "Score: 0 - 0 - 0" 16 'black)
                               (text "Other Player's Turn" 16 'black)
                               ((aw2 . grid) . draw)))

;; tests for make
(check-expect (aw1 . make 'p1 'waiting (grid% EMPTY-GRID)
                   'p1 (grid% EMPTY-GRID) 0 0 0) aw1)
(check-expect (hw1 . make 'p1 'waiting (grid% EMPTY-GRID) 'p1)
              hw1)

;; tests for on-mouse
(check-expect (hw1 . on-mouse 1 1 "button-down")
              (make-package hw1 (sub1 (ceiling (/ (add1 1) 
                                                  (* 2 PIECE-SIZE/PIXELS))))))
(check-expect (hw2 . on-mouse -1 -1 "button-down")
              hw2)

;; a default auto-world
(define default-auto-world
  (auto-world%
   false 'waiting (grid% EMPTY-GRID) 'p1
   (brain% empty) (grid% EMPTY-GRID) 0 0 0 0))

;; a default human-world
(define default-human-world
  (hum-world% false 'waiting (grid% EMPTY-GRID) 'p1))

(launch-many-worlds
   (universe (universe% false false 'p1 (new grid% EMPTY-GRID)))
   (big-bang default-auto-world)
   (big-bang default-auto-world))


#|
Introduce randomness into your computer player’s behavior.
Run two computer players against each other. How often does
the player that goes first win? How long does it take before
your computer players get good at the game? Provide empirical
data on this in your solution.

In general, Player 1 wins more. Evidence:
(Wins - Losses - Ties)
Round 1: 67 - 48 - 8
Round 2: 22 - 18 - 3
Round 3: 16 - 17 - 2
Round 4:  9 - 10 - 1
Round 5: 36 - 28 - 2
Interestingly enough, it looks like in low-scoring games
Player 2 wins more. But the longer you let it run, the more it pulls
towards player 1.
|#