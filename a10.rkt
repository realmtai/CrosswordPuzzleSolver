#lang racket

(require "puzzle.rkt")
(provide show-solution-from-file)
;; Notes for completing A10:
;;
;; * Look for comments starting with TODO to find places where you should
;;   add to this file.
;;
;; * Consider commenting out tests for functions you haven't yet implemented,
;;   so that you don't get a big pile of check-expect failures when you run
;;   your code.  As you work on individual functions, uncomment their tests.
;; 
;; * If you temporarily switch to the "Pretty Big" language level, then
;;   code that produces run-time errors will leave behind a trail of arrows
;;   representing the chain of function calls that led to the error.  That
;;   can be handy for debugging.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Low-level convenience functions for dealing with the character grid
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; A bunch of helper functions that support the functions below in the sample
;; solution were deleted from here (hint, hint).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The logic of the crisscross puzzle itself -- functions that can discover
;; legal next moves and carry them out speculatively.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; check-word: Config WordLoc Word -> Boolean
;; Determine whether the passed-in Configuration permits the
;; given word to be placed along the given list of locations.
;; Must make sure that every spot in the Grid is either open
;; (i.e., a #\#), or occupied by the letter that's at the
;; corresponding position in the word we're trying to place.
;; Examples:
;(check-expect (check-word simple-cfg (first simple-wordlocs)
;                          '(#\P #\A #\N)) true)
;(check-expect (check-word simple-cfg (first simple-wordlocs)
;                          '(#\C #\A #\K #\E)) false)

(define (check-word cfg wloc word)
  (cond 
    [(and (empty? wloc) (empty? word)) true]
    [(or (and (empty? wloc) 
              (not (empty? word))) 
         (and (empty? word) 
              (not (empty? wloc)))) false]
    [(local [(define getDaVal (getValOnIndex (config-grid cfg) 
                                             (location-row (first wloc)) 
                                             (location-col (first wloc))))] 
       (and (not (boolean? getDaVal)) (or (char=? #\# getDaVal) 
                                          (char=? (first word) getDaVal))))
     (check-word cfg (rest wloc) (rest word))]
    [else false]))

;; Tests:
;(check-expect (check-word simple-mid-cfg (first simple-wordlocs)
;                          '(#\P #\A #\N)) true)
;(check-expect (check-word simple-mid-cfg (first simple-wordlocs)
;                          '(#\C #\A #\P)) true)
;(check-expect (check-word simple-mid-cfg (first simple-wordlocs)
;                          '(#\P #\U #\N)) false)
;(check-expect (check-word simple-mid-cfg (first simple-wordlocs)
;                          '(#\P #\A #\N #\T)) false)


;getValOnIndex: (listof (listof char)) Num Num -> (union char Boolean)
;Purpose: get the element at row and col and return the element, if no element found retuen false.
;example:
;(check-expect (getValOnIndex simple-soln 1 1) #\A)

(define (getValOnIndex grid row col)
  (cond
    [(or (< row 0) (< col 0) 
         (> row (length grid)) 
         (> col (length (first grid)))) false]
    [else (list-ref (list-ref grid row) col)]
    )
  )


;test:
;(check-expect (getValOnIndex simple-soln 1 0) #\C)
;(check-expect (getValOnIndex simple-soln -11 -100) false)


;;;;;;;;;;;;;;;;;;;;;
;; replace-word: Config WordLoc Word -> Config
;; Construct the new Config that results from filling in the 
;; Grid at the specified WordLoc with the letters in the Word.  This
;; also involves removing wloc from the Config's wordlocs, and removing
;; the word itself from the Config's words.
;; Examples:
;(check-expect (replace-word
;               simple-cfg (second simple-wordlocs)(second simple-words))
;              simple-mid-cfg)
;(check-expect (replace-word
;               simple-mid-cfg (first simple-wordlocs) (first simple-words))
;              (make-config simple-soln empty empty))

(define (replace-word cfg wloc word) 
  (cond
    [(empty? wloc) cfg]
    [else (local [(define removedCfg (make-config 
                                      (doReplaceWord (config-grid cfg) 
                                                     (location-row (first wloc)) 
                                                     (location-col (first wloc)) 
                                                     (first word))
                                      (remove wloc (config-wordlocs cfg))
                                      (remove word (config-words cfg))))] 
            (replace-word removedCfg (rest wloc) (rest word)))]
    )
  )


;; Tests:
;(check-expect (replace-word (make-config simple-grid 
;                                         (list (list (make-location 1 0) (make-location 1 1) 
;                                                     (make-location 1 2) (make-location 1 3)))
;                                         (list '(#\C #\A #\K #\E)))
;                            (list (make-location 1 0) (make-location 1 1) 
;                                  (make-location 1 2) (make-location 1 3)) 
;                            '(#\C #\A #\K #\E))
;              (make-config '((#\. #\# #\. #\.)
;                             (#\C #\A #\K #\E)
;                             (#\. #\# #\. #\.))
;                           '() '()))

;doReplaceWord: (listof (listof Any)) Num Num Any -> (union (listof (listof Any)) Boolean)
;purpose: go to the number of row or col and replace the given char 
;and return the list or return false if argunment are involid.
;example:

;(check-expect (doReplaceWord simple-soln 1 0 #\H) (list
;                                                   (list #\. #\P #\. #\.)
;                                                   (list #\H #\A #\K #\E)
;                                                   (list #\. #\N #\. #\.)))

(define (doReplaceWord cfg row col oneChar)
  (cond 
    [(empty? cfg) false]
    [(= 0 row) (cons (doReplaceWord (first cfg) (- row 1) col oneChar) (rest cfg))]
    [(and (= 0 col) (> 0 row)) (cons oneChar (rest cfg))]
    [(>= row 0) (cons (first cfg) (doReplaceWord (rest cfg) (- row 1) col oneChar))]
    [(>= col 0) (cons (first cfg) (doReplaceWord (rest cfg) row (- col 1) oneChar))]
    [else false]
    )
  )


;(check-expect (doReplaceWord empty 15 10 #\S) false)


;;;;;;;;;;;;;;;;;;;;;
;; get-all-legal-words: Config WordLoc (listof Word) -> (listof Word)
;; Given a current Config cfg, a WordLoc wloc where you'd like to place
;; a word, and a list of candidate words wds, figure out which of the
;; candidate words can possibly be placed in wloc.  In a way, this is
;; the crucial step in determining the neighbours of a current Config --
;; the output of this function tells us what we should try as neighbour
;; Configs.
;; Examples:
;(check-expect (get-all-legal-words 
;               trivial-cfg empty 
;               ;; Note the use of map with string->list to save some typing.
;               (map string->list '("THIS" "SHOULD" "FAIL")))
;              empty)
;(check-expect (get-all-legal-words
;               simple-cfg (first simple-wordlocs) simple-words)
;              '((#\P #\A #\N)))

(define (get-all-legal-words cfg wloc wds)
  (foldr (lambda (x y) (cond 
                         [(check-word cfg wloc x) (cons x y)]
                         [else y])) empty wds))

;; Tests: 
;(check-expect (get-all-legal-words
;               simple-mid-cfg (first simple-wordlocs)
;               (map string->list
;                    '("PAN" "CAN" "COP" "BAT" "ANT" "SOUP" "SYRUP")))
;              (map string->list '("PAN" "CAN" "BAT")))

;;;;;;;;;;;;;;;;;;;;;
;; find-good-wordloc: Config -> WordLoc
;; Find a good location for the next word to attempt to place
;; in this Config.  Rather than choosing one at random, we look
;; through the list of WordLocs for one that already has at least
;; one previously placed letter.  That's a perfectly valid strategy,
;; and it helps make the search faster by constraining the legal
;; words that might be placed in the new position.  We need one
;; small special case -- if no words have been placed yet, we still
;; need to choose at least one WordLoc to start from.  Note that
;; the Config's list of WordLocs must be non-empty.
;; Example:
;(check-expect (find-good-wordloc simple-mid-cfg)
;              (list (make-location 0 1) (make-location 1 1)
;                    (make-location 2 1)))

(define (find-good-wordloc cfg)
  (cond
    [(empty? (config-wordlocs cfg)) empty]
    [else (local [(define (find-good-wordloc-helper cfg-wloc i re)
                    (cond
                      [(empty? cfg-wloc) re]
                      [else (local [(define dagetNumberOfExistedChar (getNumberOfExistedChar 
                                                                      (config-grid cfg) 
                                                                      (first cfg-wloc)))]
                              (cond 
                                [(>= dagetNumberOfExistedChar i) 
                                 (find-good-wordloc-helper (rest cfg-wloc) 
                                                           dagetNumberOfExistedChar 
                                                           (first cfg-wloc))]
                                [else (find-good-wordloc-helper (rest cfg-wloc) i re)]
                                )
                              )
                            ]
                      )
                    )] (find-good-wordloc-helper (config-wordlocs cfg) 0 empty))]
    )
  )

;; Tests:
;(check-expect
; (find-good-wordloc three-cfg)
; (list (make-location 2 1) (make-location 2 2) (make-location 2 3)))
;(check-expect
; (find-good-wordloc
;  (make-config
;   '((#\# #\# #\#))
;   (list (list (make-location 0 0) (make-location 0 1) (make-location 0 2)))
;   '((#\C #\A #\T))))
; (list (make-location 0 0) (make-location 0 1)
;       (make-location 0 2)))


;getNumberOfExistedChar: (listof (listof char)) (listof Location) -> Nat
;purpose: get number of char (other then #) that exist on the grid 
;example: 

;(check-expect (getNumberOfExistedChar simple-soln (first simple-wordlocs)) 3)

(define (getNumberOfExistedChar cfg listOfLoc) 
  (cond 
    [(empty? listOfLoc) 0]
    [(char=? (getValOnIndex cfg 
                            (location-row (first listOfLoc)) 
                            (location-col (first listOfLoc))) #\#) 
     (getNumberOfExistedChar cfg (rest listOfLoc))]
    [else (+ 1 (getNumberOfExistedChar cfg (rest listOfLoc)))]
    )
  )

;example:
;
;(check-expect (getNumberOfExistedChar simple-soln (second simple-wordlocs)) 4)
;(check-expect (getNumberOfExistedChar simple-soln empty) 0)

;;;;;;;;;;;;;;;;;;;;;
;; neighbours: Config -> (listof Config)
;; Given a current Config, find all the neighbouring Configs you can
;; pass to by placing any of the eligible words (in the sense of
;; get-all-legal-words above) in the first eligible WordLoc found
;; (in the sense of find-good-wordloc).
;; Examples:
;(check-expect (neighbours simple-cfg) (list simple-mid-cfg))

(define (neighbours cfg)
  (local [(define da-good-wordloc 
            (find-good-wordloc cfg))]
    (map (lambda (word) (replace-word cfg da-good-wordloc word)) 
         (get-all-legal-words cfg da-good-wordloc 
                              (config-words cfg)))))

;; Tests:
;(check-expect (neighbours simple-mid-cfg)
;              (list
;               (make-config '((#\. #\P #\. #\.)
;                              (#\C #\A #\K #\E)
;                              (#\. #\N #\. #\.))
;                            empty empty)))
;; An admittedly convoluted test to save typing in explicit Configs.
;; If you're confused, try copying the entire result of the test (the
;; map expression) into the interactions window.
;(check-expect
; (neighbours 
;  (make-config
;   '((#\# #\# #\#))
;   (list (list (make-location 0 0) (make-location 0 1) (make-location 0 2)))
;   (map string->list '("CAT" "DOG" "HAT" "TOT"))))
; (local [(define lst (map string->list '("CAT" "DOG" "HAT" "TOT")))]
;   (map (lambda (wd) (make-config (list wd) empty (remove wd lst))) lst)))
;(check-expect
; (neighbours three-cfg)
; (map (lambda (wd) 
;        (make-config
;         (list (first three-grid) (second three-grid) (cons #\. wd))
;         (remove (list (make-location 2 1) (make-location 2 2)
;                       (make-location 2 3))
;                 three-wordlocs)
;         (remove wd three-words)))
;      (filter (lambda (x) (= (length x) 3)) three-words)))

;;;;;;;;;;;;;;;;;;;;;
;; search: Config -> (union Grid false)
;; Solve a crisscross puzzle, given the initial configuration of the
;; puzzle (an empty board, a full word list, and a list of word
;; locations).  The code is similar, but not identical, to find-route
;; and find-route/list about a third of the way through the Module 12
;; lecture notes.  
;; Examples:
;(check-expect (search trivial-cfg) trivial-grid)
;(check-expect (search simple-cfg) simple-soln)

(define (search cfg)
  ;; You should not need to modify this function; if you correctly
  ;; implement neighbours above, this function will do the right thing
  ;; with it.
  (local
    [;; find-route: Config -> (union Grid false)
     ;; Search outward from this configuration to see if there's a path
     ;; to a solution.
     (define (find-route cfg)
       (cond
         [(empty? (config-wordlocs cfg)) (config-grid cfg)]
         [else (find-route/list (neighbours cfg))]))
     
     ;; find-route/list: (listof Config) -> (union Grid false)
     ;; Search outward from every configuration in the passed-in list of
     ;; configurations.  If any one of them leads to a solution, stop and
     ;; produce that solution.  Produce false if you run out of options.
     (define (find-route/list locfg)
       (cond
         [(empty? locfg) false]
         [else
          (local
            [(define cur (find-route (first locfg)))]
            (cond
              [(not (boolean? cur)) cur]
              [else (find-route/list (rest locfg))]))]))]
    (find-route cfg)))

;; Tests:
;(check-expect (search (make-config simple-grid simple-wordlocs empty)) false)
;(check-expect (search three-cfg) three-soln)

;;;;;;;;;;;;;;;;;;;;;
;; extract-words: Grid -> (listof WordLoc)
;; Given a grid of Characters representing an empty crisscross puzzle,
;; identify the positions of all the words that must be filled in.
;; Examples:
;(check-expect (extract-words trivial-grid) empty)
;(check-expect 
; (lists-equiv? (extract-words simple-grid) simple-wordlocs equal?)
; true)

(define (extract-words grid)
  (append (getRawGridLocOnHiz grid) 
          (switchLoc (getRawGridLocOnHiz 
                      (inverse grid (make-list 
                                     (length (first grid)) empty))))))
;
;(check-expect 
; (lists-equiv? (extract-words three-grid) three-wordlocs equal?)
; true)

; switchLoc: (listof (listof Location)) -> (listof (listof Location))
; purpose: switch the x y vale
; exmaple 


;(check-expect (switchLoc three-wordlocs) (list
;                                          (list
;                                           (make-location 1 0)
;                                           (make-location 2 0)
;                                           (make-location 3 0))
;                                          (list
;                                           (make-location 1 0)
;                                           (make-location 1 1)
;                                           (make-location 1 2))
;                                          (list
;                                           (make-location 2 0)
;                                           (make-location 2 1)
;                                           (make-location 2 2))
;                                          (list
;                                           (make-location 3 0)
;                                           (make-location 3 1)
;                                           (make-location 3 2))
;                                          (list
;                                           (make-location 0 1)
;                                           (make-location 1 1)
;                                           (make-location 2 1)
;                                           (make-location 3 1))
;                                          (list
;                                           (make-location 1 2)
;                                           (make-location 2 2)
;                                           (make-location 3 2))))

(define (switchLoc lococ)
  (cond
    [(empty? lococ) empty]
    [else (cons (map (lambda (x) (make-location (location-col x) (location-row x))) 
                     (first lococ)) 
                (switchLoc (rest lococ)))]))

;
;(check-expect (switchLoc (list(list
;                               (make-location 1 0)
;                               (make-location 2 0)
;                               (make-location 3 0))
;                              (list
;                               (make-location 1 0)
;                               (make-location 1 1)
;                               (make-location 1 2))
;                              (list
;                               (make-location 2 0)
;                               (make-location 2 1)
;                               (make-location 2 2))
;                              (list
;                               (make-location 3 0)
;                               (make-location 3 1)
;                               (make-location 3 2))
;                              (list
;                               (make-location 0 1)
;                               (make-location 1 1)
;                               (make-location 2 1)
;                               (make-location 3 1))
;                              (list
;                               (make-location 1 2)
;                               (make-location 2 2)
;                               (make-location 3 2)))) three-wordlocs)
;
;(check-expect (switchLoc empty) empty)

;inverse: (listof (listof Any)) (listof Any) -> (listof (listof Location))
;purpose: do matrix inverse on the loa return the inversed loa
;example

;(check-expect (inverse simple-soln (make-list (length (first simple-soln)) empty)) (list
;                                                                                    (list #\. #\C #\.)
;                                                                                    (list #\P #\A #\N)
;                                                                                    (list #\. #\K #\.)
;                                                                                    (list #\. #\E #\.)))

(define (inverse loa placeHolder)
  (cond 
    [(empty? loa) placeHolder]
    [else (append-list (first loa) (inverse (rest loa) placeHolder))]))

;test:
;(check-expect (inverse (list
;                        (list #\. #\C #\.)
;                        (list #\P #\A #\N)
;                        (list #\. #\K #\.)
;                        (list #\. #\E #\.)) (make-list (length (first (list
;                                                                       (list #\. #\C #\.)
;                                                                       (list #\P #\A #\N)
;                                                                       (list #\. #\K #\.)
;                                                                       (list #\. #\E #\.)))) empty)) simple-soln)


;append-list: (listof Any) (listof Any) -> (union (listof (listof Any)) boolean)
;purpose: to zip up 2 list return it.
;example: 

;(check-expect (append-list (first simple-soln) 
;                           (make-list (length (first simple-soln)) empty)) 
;              (list
;               (list #\.)
;               (list #\P)
;               (list #\.)
;               (list #\.)))

(define (append-list list1 list2)
  (cond [(and (empty? list1) (empty? list2)) empty]
        [(or (and (empty? list1) (not (empty? list2))) 
             (and (empty? list2) (not (empty? list1)))) false]
        [else (cons (cons (first list1) (first list2))
                    (append-list (rest list1) (rest list2)))]))

;;test:
;(check-expect (append-list empty empty) empty)


;getRawGridLocOnHiz: (listof (listof Char)) -> (listof (listof Location))
;purpose: convert grid in to list of loc and return it.
;example:

;(check-expect (getRawGridLocOnHiz three-grid) (list
;                                               (list
;                                                (make-location 0 1)
;                                                (make-location 0 2)
;                                                (make-location 0 3))
;                                               (list
;                                                (make-location 1 0)
;                                                (make-location 1 1)
;                                                (make-location 1 2)
;                                                (make-location 1 3))
;                                               (list
;                                                (make-location 2 1)
;                                                (make-location 2 2)
;                                                (make-location 2 3))))

(define (getRawGridLocOnHiz grid)
  (local [(define rarGrid (map (lambda (x) (map (lambda (xx) 
                                                  (cond 
                                                    [(char=? (getValOnIndex grid (first x) (first xx)) #\.) 
                                                     'holder]
                                                    [else (make-location (first x) (first xx))]))
                                                (build-list (length (first grid)) list)))
                               (build-list (length grid) list)))
          ;computeRawLocGrid: (listof (listof Location))-> (listof (listof Location))
          ;purpose: compute the raw locations in to finial locations and return it.
          (define (computeRawLocGrid locs) (cond [(empty? locs) empty]
                                                 [else (append (getallTokenIntoList (first locs)) 
                                                               (computeRawLocGrid (rest locs)))]))]
    (computeRawLocGrid rarGrid)))

;tests:


;getallTokenIntoList: (listof Location) -> (listof (listof Location))
;purpose: get one row of raw locations and convert into final formate and return it.
;example:
;(check-expect (getallTokenIntoList (list (make-location 0 1) (make-location 0 2) (make-location 0 3) 
;                                         'holder 
;                                         (make-location 0 1) (make-location 1 1) (make-location 2 1)))
;              (list
;               (list
;                (make-location 0 1)
;                (make-location 0 2)
;                (make-location 0 3))
;               (list
;                (make-location 0 1)
;                (make-location 1 1)
;                (make-location 2 1))))

(define (getallTokenIntoList mylist)
  (local [(define daEle (getFirstTokenInDaRow mylist true))]
    (cond 
      [(empty? mylist) empty]
      [(or (and (symbol? (first daEle)) 
                (symbol=? (first daEle) 'holder)) 
           (<= (length daEle) 1))
       (getallTokenIntoList (rest mylist))]
      [else (cons daEle (getallTokenIntoList 
                         (foldr (lambda (x y) (remove x y)) 
                                mylist 
                                daEle)))])))
;;test:
;(check-expect (getallTokenIntoList empty) empty)

;getFirstTokenInDaRow (listof Location) Boolean -> (listof Location)
;purpose: get the first token in the row.
;example:
;(check-expect (getFirstTokenInDaRow (list (make-location 0 1) (make-location 0 2) (make-location 0 3)
;                                          'holder 
;                                          (make-location 0 1) (make-location 1 1) (make-location 2 1)) true) 
;              (list (make-location 0 1)
;                    (make-location 0 2)
;                    (make-location 0 3)))



(define (getFirstTokenInDaRow row testFirstEle)
  (cond [(empty? row) empty]
        [(and testFirstEle 
              (symbol? (first row)) 
              (symbol=? (first row) 'holder)) '(holder)]
        [(and (symbol? (first row))
              (symbol=? (first row) 'holder)) empty]
        [else (cons (first row)
                    (getFirstTokenInDaRow (rest row) false))]))


;test: 
;
;(check-expect (getFirstTokenInDaRow empty true) empty)





;;;;;;;;;;;;;;;;;;;;;
;; solve-crisscross: Puzzle -> Grid
;; Given a Puzzle, as produced by read-puzzle, solve the puzzle by producing
;; a final, filled-in Grid if there is a solution, or false if there isn't.
;; Examples:
;(check-expect
; (solve-crisscross 
;  (make-puzzle '("###") '("CAT"))) '((#\C #\A #\T)))

(define (solve-crisscross puzz)
  ;; TODO
  ;; A simple wrapper that prepares an initial Config from puzz, and
  ;; passes it to the search function.
  
  
  ;'NOT-IMPLEMENTED
  (local [(define grid (map string->list (puzzle-lines puzz)))
          (define cfg-format-words (map string->list (puzzle-words puzz)))
          ]
    (search (make-config 
             grid
             (extract-words grid)
             cfg-format-words))
    )
  )

;; Tests:
;(check-expect
; (solve-crisscross
;  (make-puzzle '("####" "#..#" "####") '("DOGS" "DOT" "SAP" "TARP")))
; '((#\D #\O #\G #\S) (#\O #\. #\. #\A) (#\T #\A #\R #\P)))
;(check-expect
; (solve-crisscross
;  (make-puzzle '(".###" "####" ".###") 
;               '("FOR" "MANE" "BED" "FAB" "ONE" "RED")))
; '((#\. #\F #\O #\R) (#\M #\A #\N #\E) (#\. #\B #\E #\D)))

;;;;;;;;;;;;;;;;;;;;;
;; show-solution-from-file: String -> Void
;; A handy do-everything function.  Read a puzzle from a file,
;; solve it, and display the solution.

(define (show-solution-from-file fname)
  (show-board (solve-crisscross (read-puzzle fname))))

;; Tests:
;; The function produces Void and prints its output as a side effect,
;; so there's no point testing it via check-expect.
;(define rt (map 
;            (lambda (x) 
;              (time (show-solution-from-file x)))
;            '("puzzle01.txt"
;              "puzzle02.txt"
;              "puzzle03.txt"
;              "puzzle04.txt"
;              "puzzle05.txt"
;              "puzzle06.txt"
;              "puzzle07.txt"
;              "puzzle08.txt"
;              "puzzle09.txt"
;              "puzzle10.txt")))
