#lang class/1
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


;====================t
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

;===========
; Classes ||
;===============================================================================

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
  
  
  ;; won? : Piece -> Boolean
  ;; Checks if the given piece has the number in a row needed
  ;; to win.
  (define (won? player)
    (or (ormap (lambda(y)
                 (or (x-in-a-row? player TO-WIN (walk 0 y 1 0))
                     (x-in-a-row? player TO-WIN (walk 0 y 1 1))
                     (x-in-a-row? player TO-WIN (walk 0 y 1 -1))))
               (build-list BOARD-HEIGHT/PIECES (lambda(y)y)))
        (ormap (lambda(x)
                 (x-in-a-row?
                  player TO-WIN (walk x (sub1 BOARD-HEIGHT/PIECES) 1 -1)))
               (build-list BOARD-WIDTH/PIECES (lambda(x)x)))
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
  ;; Determines if there is room to place a piece in a column
  (define (can-place? col)
    (not (false? (member 'mt (list-ref (field grid) col))))))


(define-class server-grid%
  (super grid%)
  
  ;; place-piece : Number Piece -> grid%
  ;; Places a piece in the given column
  (define (place-piece column piece)
    (new server-grid%
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
                (list-ref (field grid) cur-col)))))))

#|
TESTS ARE UNNECESSARY AS ALL THE CODE IS IDENTICAL TO THE OTHER FILE

(define testg1 (list
                (list 'mt 'p1 'p1 'p1 )
                (list 'p2 'p2 'p1 'p1)
                (list 'mt 'mt 'mt 'mt)
                (list 'mt 'mt 'mt 'mt)))
(define testg2 (list
                (list 'mt 'p2 'p1 'p1 )
                (list 'p2 'p1 'p1 'p2)
                (list 'p1 'p2 'mt 'p2)
                (list 'mt 'mt 'mt 'mt)))
(define testg3 (list
                (list 'mt 'mt 'mt 'p1 )
                (list 'mt 'mt 'mt 'p1)
                (list 'mt 'mt 'mt 'p1)
                (list 'mt 'mt 'mt 'p1)))

;; tests for won?
(check-expect ((new server-grid% testg1) . won? 'p1) true)
(check-expect ((new server-grid% testg1) . won? 'p2) false)
(check-expect ((new server-grid% testg2) . won? 'p1) true)
(check-expect ((new server-grid% testg2) . won? 'p2) false)
(check-expect ((new server-grid% testg3) . won? 'p1) true)
(check-expect ((new server-grid% testg3) . won? 'p2) false)
;; tests for walk and x-in-a-row? are tested by won?'s tests

;; tests for get-piece-at
(check-expect ((new server-grid% testg1) . get-piece-at 0 0) 'mt)
(check-expect ((new server-grid% testg1) . get-piece-at 1 1) 'p2)
(check-expect ((new server-grid% testg1) . get-piece-at 1 2) 'p1)
(check-expect ((new server-grid% testg2) . get-piece-at 2 3) 'p2)
(check-expect ((new server-grid% testg2) . get-piece-at 2 0) 'p1)
(check-expect ((new server-grid% testg2) . get-piece-at 3 2) 'mt)

;; tests for can-place? 
(check-expect ((new server-grid% testg1) . can-place? 0) true)
(check-expect ((new server-grid% testg1) . can-place? 1) false)
(check-expect ((new server-grid% testg1) . can-place? 2) true)
(check-expect ((new server-grid% testg2) . can-place? 1) false)
(check-expect ((new server-grid% testg2) . can-place? 0) true)
(check-expect ((new server-grid% testg3) . can-place? 3) true)
|#

(define-class client-grid%
  (super grid%)
  
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
  ;; Draws this client-grid%
  (define (draw)
    (foldl (lambda (column scn)
             (beside scn (draw-col column)))
           (draw-col (first (field grid)))
           (rest (field grid)))))


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
            (new universe% (field p1) iw 'p1 (field grid))
            (list (make-mail iw (list 'assignment 'p2)))
            empty)]
          [else (make-bundle this empty (list iw))]))
  
  (define (game-over?)
    (cond
      [(send (field grid) won? 'p1) 'p1]
      [(send (field grid) won? 'p2) 'p2]
      [(not (send (field grid) availability?)) 'tie]
      [else 'game-on]))
  
  (define (next-turn)
    (if (symbol=? (field turn) 'p1) 'p2 'p1))
  
  (define (iw->symbol iw)
    (if (equal? iw (field p1)) 'p1 'p2))
  
  (define (mail-both data)
    (list (make-mail (field p1) data)
          (make-mail (field p2) data)))
  
  (define (on-msg iw column-choice)
    (local [(define game-state (game-over?))]
      (cond 
        ;; if it's not this player's turn, ignore the request
        [(not (if (equal? (field turn) 'p1)
                  (equal? iw (field p1))
                  (equal? iw (field p2)))) (make-bundle this empty empty)]
        [(not (symbol=? game-state 'game-on))
         (make-bundle this (mail-both (list 'game-over game-state)))]
        [((field grid) . can-place? column-choice)
         (local
           [(define new-uni (new universe%
                                 (field p1)
                                 (field p2)
                                 (next-turn)
                                 ((field grid) . place-piece column-choice (iw->symbol iw))))
            (define new-game-state (send new-uni game-over?))]
           (make-bundle
            new-uni 
            (append (mail-both (list 'grid (list ((send new-uni grid) . grid) (send new-uni turn))))
                    (mail-both
                     (if (symbol=? new-game-state 'game-on)
                         (list 'turn (next-turn))
                         (list 'game-over new-game-state))))
            empty))]
        [else (make-bundle this empty empty)]))))



(define-class world%
  (fields player status grid turn)
  
  (define (register) "127.0.0.1")
  
  (define (to-draw)
    (above
     (cond [(equal? (field player) 'p1) (text "Player 1" 20 'red)]
           [(equal? (field player) 'p2) (text "Player 2" 20 'black)]
           [(false? (field player)) (text "Waiting to start..." 20 'black)])
     (text (cond 
             [(and (equal? (field status) 'game-over) (not (equal? (field player) (field turn))))
              "You won"]
             [(and (equal? (field status) 'game-over) (equal? (field player) (field turn)))
              "You Lost"]
             [(and (not (false? (field player)))
                   (equal? (field player) (field turn))) "Your Turn"]
             [else "Other Player's Turn"])
           16
           'black)
     ((field grid) . draw)))
  
  (define (on-mouse x y m)
    (cond
      [(and (string=? m "button-down")
            (equal? (field turn) (field player)))
       (make-package this (sub1 (ceiling (/ (add1 x) (* 2 PIECE-SIZE/PIXELS)))))]
      [else this]))
  
  
  (define (on-receive d)
    (local [(define request (first d))
            (define data (second d))]
      (cond
        [(symbol=? request 'assignment)
         (new world% data (field status) (field grid) (field turn))]
        [(symbol=? request 'grid)
         (new world% (field player) 'game-on (new client-grid% (first data)) (second data))]
        [(symbol=? request 'game-over)
         (stop-with
          (new world% (field player) 'game-over (field grid) (field turn)))]
        [else this]))))

(launch-many-worlds
 (universe (new universe% false false 'p1 (new server-grid% EMPTY-GRID)))
 (big-bang (new world% false 'waiting (new client-grid% EMPTY-GRID) 'p1))
 (big-bang (new world% false 'waiting (new client-grid% EMPTY-GRID) 'p1)))