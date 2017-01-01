(load "util")
(load "solve")

(define (string-split str)
  (define (iter str index result)
    (let ((len (string-length str)))
      (if (= len 0)
        result
        (if (= len index)
          (append result (list str))
          (if (char=? (string-ref str index) #\space)
            (if (= index 0)
              (iter (substring str 1 len) 0 result)
              (iter (substring str index len) 0 (append result (list (substring str 0 index)))))
            (iter str (+ index 1) result))))))
  (iter str 0 '()))

(define (read-datas n)
  (define (iter n result)
    (if (= n 0)
      result
      (iter (- n 1) (append result (list (map string->number (string-split (read-line))))))))
  (iter n '()))

(define r (read))
(define c (read))
(read-line)          ; clear input stream
(define r-datas (read-datas r))
(define c-datas (read-datas c))

(define datas
  (list
    (list r c)
    r-datas
    c-datas))

(define (display-solutions solutions)
  (define (display-solution solution)
    (define (display-line line)
      (define (display-cell cell)
        (if (= cell false)
          (display "..")
          (display "##")))
      (begin
        (map display-cell line)
        (newline)))
    (begin
      (map display-line solution)
      (newline)))
  (map display-solution solutions))


(define solutions (solve-nonogram datas))
(display-solutions solutions)
