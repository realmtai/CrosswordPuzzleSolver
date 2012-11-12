(module puzzle (lib "plt-pretty-big-text.ss" "lang")
  
  ;; Do not make changes to this file!
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Data and type definitions
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-struct puzzle (lines words) #f)
  ;; A Puzzle is (make-puzzle (listof String) (listof String))
  ;; The puzzle struct holds the input to a crisscross puzzle -- a visual
  ;; description of the grid of words, together with the list of words (or
  ;; possibly a superset of that list) that fit into the grid.
  
  (define-struct location (row col) #f)
  ;; A Location is a structure (make-location Nat Nat)
  ;; It records, in order, the row and column of a location in a grid of
  ;; letters.
  
  ;; Three convenient data definitions to simplify contracts below:
  ;; A Word is a (listof Character)
  ;; A WordLoc is a (listof Location) (the set of locations associated with
  ;;   a word in the grid)
  ;; A Grid is a (listof (listof Character)
  
  (define-struct config (grid wordlocs words) #f)
  ;; A Config is a structure
  ;;   (make-config Grid (listof WordLoc) (listof Word))
  ;; The first field holds the current set of all characters in the grid.
  ;; It consists of a list of equal-length lists of characters.  Each
  ;; character is a period #\. (not part of a word), a hash #\# (a spot
  ;; in a word that doesn't have a letter yet), or a capital letter.
  ;; The second field is a list of cell Locations for each word.  The
  ;; list is given in left-to-right or top-to-bottom order.  The third
  ;; field contains the actual words to be placed, each exploded into a
  ;; list of characters.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Utility functions
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; read-puzzle: String -> Puzzle
  ;; Read a crisscross puzzle from a named file.  Produce a list of two items:
  ;;  (1) The complete drawing of the word grid (represented as a list 
  ;;      of strings, made up of #\. (empty space) or #\# (a cell where 
  ;;      a letter will go)
  ;;  (2) The list of actual words to fit into the grid.
  (define (read-puzzle fname) 
    (local
      [;; read-until-puzzle-size: File -> Nat
       ;; Discard comment lines in the input file, then read and return
       ;; the value indicating the number of rows in the puzzle.
       (define (read-until-puzzle-size in)
         (local
           [(define line (read-line in))]
           (cond
             [(char=? (string-ref line 0) #\;) (read-until-puzzle-size in)]
             [else (string->number line)])))
       
       ;; lines: File -> Puzzle
       ;; Main routine for processing a file.  Basically use the known
       ;; structure of the file to read any initial comments, then each
       ;; of the parts of the puzzle's description in turn.
       (define (lines in)
         (local [(define num-lines (read-until-puzzle-size in))
                 (define lines 
                   (build-list num-lines (lambda (x) (read-line in))))
                 (define num-words (string->number (read-line in)))
                 (define words 
                   (build-list num-words (lambda (x) (read-line in))))]
           (make-puzzle lines words)))]
      (call-with-input-file fname lines)))
  
  ;; show-board: (listof (listof Character)) -> Void
  ;; Consume a grid of characters in a list of lists and display it.  All
  ;; periods are replaced by spaces, with the intention of showing just the
  ;; final letters in the filled-in grid.
  (define (show-board grid)
    (local
      [(define (dot->space ch)
         (cond
           [(char=? ch #\.) #\space]
           [else ch]))]
      (cond
        [(empty? grid) (void)]
        [else 
         (begin
           (display (list->string (map dot->space (first grid))))
           (display "\n")
           (show-board (rest grid)))])))
  
  ;; lists-equiv?: (listof X) (listof Y) (X Y -> boolean) -> boolean
  ;; Given two lists l1 and l2, determine whether the lists are essentially
  ;; the same up to reordering, where equivalence of individual elements
  ;; from the two lists is given from a passed-in predicate.  This function
  ;; will work as long as one of the two lists contains no duplicates.
  ;; Useful in tests where we don't care about ordering.
  
  ;; Examples:
  ;  (check-expect 
  ;    (lists-equiv? '(1 2 3) '("2" "3" "1") 
  ;                  (lambda (x y) (string=? (number->string x) y)))
  ;    true)
  ;  (check-expect (lists-equiv? '(1 2 3 4) '(2 3 4 5) =) false)
  
  (define (lists-equiv? l1 l2 comp)
    ;; The approach is a bit sneaky, but very succinct.  Check that
    ;; every element of l1 appears somewhere in l2 (in terms of comp),
    ;; and that every elements of l2 appears somewhere in l1.
    (and (= (length l1) (length l2))
         (andmap (lambda (x1) (ormap (lambda (x2) (comp x1 x2)) l2)) l1)
         (andmap (lambda (x2) (ormap (lambda (x1) (comp x1 x2)) l1)) l2)))
  
  ;; Tests:
  ;  (check-expect (lists-equiv? '(1 2 3 4 5) '(5 2 4 1 3) =) true)
  ;  (check-expect (lists-equiv? '(1 5 10 15 20) '(21 16 9 4 0)
  ;                              (lambda (x y) (< (abs (- x y)) 2))) true)
  ;  
  
  ;; This section is part of Scheme's module system.  All the names
  ;; in the list below are exported from this file to any file that
  ;; includes it via a require line.
  (provide
   make-puzzle
   puzzle-lines
   puzzle-words
   
   make-location
   location?
   location-row
   location-col
   
   make-config
   config?
   config-grid
   config-wordlocs
   config-words
   
   read-puzzle
   show-board
   lists-equiv?))
