(define (solve-line line-data data)
  (define (get-all-cases line-length data)
    (define (get-case-data n k)
      (define (select-num n)
        (define (iter n result)
          (if (zero? n)
            (append result (list n))
            (iter (- n 1) (append result (list n)))))
        (iter n '()))
      (define (iter n k)
        (if (zero? n)
          (list '())
            (apply append
              (map
                (lambda (dk)
                  (map
                    (lambda (tail) (append (list dk) tail))
                    (iter (- n 1) (- k dk))))
                (select-num k)))))
      (define cases (iter n k))
      (map
        (lambda (__case-data)
          (append (list (car __case-data)) (map (lambda (x) (+ x 1)) (cdr __case-data))))
          cases))
    (define (generate-line-data-from-case-data case-data)
      (define (iter data case-data result)
        (if (null? data)
          (append result (repeat-item false (- line-length (length result))))
          (iter (cdr data) (cdr case-data) (append result (repeat-item false (car case-data)) (repeat-item true (car data))))))
      (iter data case-data '()))
    (map generate-line-data-from-case-data (get-case-data (length data) (- line-length (least-length data)))))
  
  (define (collect-available-cases cases)
    (define (available? the-case)
      (define (iter line-data the-case)
        (if (null? line-data)
          #t
          (if (= (car line-data) undefined)
            (iter (cdr line-data) (cdr the-case))
            (if (= (car line-data) (car the-case))
              (iter (cdr line-data) (cdr the-case))
              #f))))
      (iter line-data the-case))
    (define (iter cases result)
      (if (null? cases)
        (if (null? result)
          (list '())
          result)
        (if (available? (car cases))
          (iter (cdr cases) (append result (list (car cases))))
          (iter (cdr cases) result))))
    (iter cases '()))
    
  (define (list-and cases)
    (define (items-are-equals? the-list)
      (if (= (length the-list) 1)
        #t
        (if (= (car the-list) (cadr the-list))
          (items-are-equals? (cdr the-list))
          #f)))
    (define (iter cases result)
      (if (null? (car cases))
        result
        (if (items-are-equals? (map car cases))
          (iter (map cdr cases) (append result (list (caar cases))))
          (iter (map cdr cases) (append result (list undefined))))))
    (iter cases '()))
  
  (list-and
    (collect-available-cases
      (get-all-cases (length line-data) data))))

(define (solve-nonogram datas)
  (define (range n)
    (define (iter n result)
      (if (= n 0)
        (append result (list n))
        (iter (- n 1) (append result (list n)))))
    (iter (- n 1) '()))
  
  (define (solve board queue1 queue2 changed)
    (if (null? queue1)
      (if (null? queue2)
        (list board)
        (if changed
          (solve board queue2 '() #f)
          (let ((r (cdar queue2)))         ;guessing
            (let ((c (index-of-item (r-ref board r) undefined)))
              (append
                (solve
                  ((lambda (board)
                    (define _board (copy-board board))
                    (begin
                      (set-car! (list-tail (list-ref _board r) c) true)
                      _board)) board)
                  queue2
                  '()
                  #f)
                (solve
                  ((lambda (board)
                    (define _board (copy-board board))
                    (begin
                      (set-car! (list-tail (list-ref _board r) c) false)
                      _board)) board)
                  queue2
                  '()
                  #f))))))
      (let ((target (car queue1)))
        (if (car target)
          (let
            (
              (data          (list-ref (cadr datas) (cdr target)))
              (line-data-old (r-ref board (cdr target))))
            (let ((line-data     (solve-line line-data-old data)))
              (if (null? line-data)
                '()
                (solve
                  (let ((_board board))
                    (set-r! _board (cdr target) line-data)
                    _board)
                  (cdr queue1)
                  (if (complete? line-data)
                    queue2
                    (append queue2 (list target)))
                  (or changed (not (equal? line-data-old line-data)))))))
          (let
            (
              (data          (list-ref (caddr datas) (cdr target)))
              (line-data-old (c-ref board (cdr target))))
            (let ((line-data     (solve-line line-data-old data)))
              (if (null? line-data)
                '()
                (solve
                  (let ((_board board))
                    (set-c! _board (cdr target) line-data)
                    _board)
                  (cdr queue1)
                  (if (complete? line-data)
                    queue2
                    (append queue2 (list target)))
                  (or changed (not (equal? line-data-old line-data)))))))))))
  
  (define r-num (caar  datas))
  (define c-num (cadar datas))
  (define target-list
    (append
      (map (lambda (n) (cons #t n)) (range r-num))
      (map (lambda (n) (cons #f n)) (range c-num))))
  
  (solve (make-board r-num c-num) target-list '() #f))
