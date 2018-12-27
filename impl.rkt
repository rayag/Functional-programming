#lang racket

(provide winner
         play)

(require "hw2-62013.rkt")

(define (id x) x)

(define (winner b)
  ; returns a sign if all elements are equal to it
  ; returns #f if there is at least one #f in the list
  ; returns "D" if there is no #f in the array and all other elements are not equal
  (define (has-winner xs)
    (define candidate (car xs)) 
    (cond ((not (andmap id xs)) #f)
          ((all? (lambda (x) (eq? x candidate)) xs) candidate)
          (else "D")))

  (let* ((row (list (rows b)))   ; all rows                         
         (col (list (cols b)))   ; all cols 
         (diag (list (diags b))) ; both diagonals
         (components (concat (append row col diag)))    ; rows + cols + diags
         (win-comp (map has-winner components)))        ; winners from each component (row, col, diagonal)
    (cond ((any? (lambda(x) (eq? x "O")) win-comp) "O")
          ((any? (lambda(x) (eq? x "X")) win-comp) "X")
          (else (andmap id win-comp)))))




; Returns list of pairs each of which represents a possible position that a sign could be placed on board b
(define (possible-positions b)
  (define rows (length b))
  (define cols (length (car b)))
  (define (help i j)
    (cond ((>= i rows) '())                                             ; we've reached the end of the matrix
          ((>= j cols) (help (+ i 1) 0))                                ; we've reached the end of the line
          ((not (matrix-ref b i j)) (cons (cons i j) (help i (+ j 1)))) ; element is a possible position, so add it to the ist
          (else (help i (+ j 1)))))                                     ; element is NOT a possible position, so it is not added
  (help 0 0))

(define (oponent x)
  (if (eq? x "X")
      "O"
      "X"))

; Evalutates a specific position on the given board
(define (eval-position board position player me)
  (let* ((row (car position))
         (col (cdr position))
         (new-board (place board row col player)) ; board with "player" put on "position"
         (outcome (winner new-board)))            ; outcome of the game: "X" - X has won, "O" - O has won, "D" - draw, #f game has not still ended
    (if me
        (cond ((eq? outcome player) 1)            ; this position is winning for current player
              ((eq? outcome "D") 0)               ; this position ends up with a draw
              (else (minimum (map (lambda(position) (eval-position new-board position (oponent player) #f)) (possible-positions new-board)))))
        (cond ((eq? outcome player) -1)           ; this position is winning for the current player (minimizer)
              ((eq? outcome "D") 0)
              (else (maximum (map (lambda(position) (eval-position new-board position (oponent player) #t)) (possible-positions new-board))))))))

; returns max(xs)
(define (maximum xs)
  (foldl max (car xs) (cdr xs)))

; returns min(xs)
(define (minimum xs)
  (foldl min (car xs) (cdr xs)))


; Returns x: f(x) = max{ f(xi) | xi is from xs}
(define (maximum-on f xs)
  (foldl (lambda (u v) (if (> (f u) (f v)) u v)) (car xs) (cdr xs)))


; Naive "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
(define (play curr-board curr-sign)
  (let ((curr-player (if curr-sign "X" "O")))                                                                   ; identify which the current sign is
    (maximum-on (lambda (pos) (eval-position curr-board pos curr-player #t)) (possible-positions curr-board)))) ; play the best move for current player

