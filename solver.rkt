#lang racket

(require "a10.rkt")
(define rt (map 
            (lambda (x) 
              (time (show-solution-from-file x)))
            '("puzzle01.txt"
              "puzzle02.txt"
              "puzzle03.txt"
              "puzzle04.txt"
              "puzzle05.txt"
              "puzzle06.txt"
              "puzzle07.txt"
              "puzzle08.txt"
              "puzzle09.txt"
              "puzzle10.txt")))