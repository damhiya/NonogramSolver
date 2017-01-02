(define undefined 0)    ; constant
(define false    -1)    ; constant
(define true      1)    ; constant

(define (repeat-item value n) ; Test Passed
  (define (iter n result)
    (if (zero? n)
      result
      (iter (- n 1) (append result (list value)))))
  (iter n '()))

(define (make-board r-num c-num) ; Test Passed
  (define (iter r result)
    (if (zero? r)
      result
      (iter (- r 1) (cons (repeat-item undefined c-num) result))))
  (iter r-num '()))

(define (r-ref board r) (list-ref board r)) ; Test Passed

(define (c-ref board c) ; Test Passed
  (define (iter board result)
    (if (null? board)
      result
      (iter (cdr board) (append result (list (list-ref (car board) c))))))
  (iter board '()))

(define (set-r! board r line-data) ; Test Passed
  (set-car! (list-tail board r) line-data))

(define (set-c! board c line-data) ; Test Passed
  (define (iter board line-data)
    (if (not (null? line-data))
      (begin
        (set-car! (list-tail (car board) c) (car line-data))
        (iter (cdr board) (cdr line-data))
        (void))))
  (iter board line-data))

(define (list-and list1 list2)
  (define (iter list1 list2 result)
    (if (null? list1)
      result
      (let ((var (car list1)))
        (if (= var (car list2))
          (iter (cdr list1) (cdr list2) (append result (list var)))
          (iter (cdr list1) (cdr list2) (append result (list undefined)))))))
  (iter list1 list2 '()))

(define (least-length data) (+ (apply + data) (length data) -1)) ; Test Passed

(define (complete? line-data)
  (if (null? line-data)
    #t
    (if (= (car line-data) undefined)
      #f
      (complete? (cdr line-data)))))

(define (index-of-item line-data item) ; Test Passed
  (define (iter line-data i)
    (if (null? line-data)
      -1
      (if (= (car line-data) item)
        i
        (iter (cdr line-data) (+ i 1)))))
  (iter line-data 0))
  
(define (copy-list the-list)
  (define (iter the-list result)
    (if (null? the-list)
      result
      (iter (cdr the-list) (cons (car the-list) result))))
  (iter the-list '()))

(define (copy-board board)
  (define (iter board result)
    (if (null? board)
      result
      (iter (cdr board) (cons (copy-list (car board)) result))))
  (iter board '()))
