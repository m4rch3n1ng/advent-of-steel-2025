(require "steel/time/time.scm")
(require "util/quicksort.scm")

(struct point (x y z) #:transparent)

(define (connections boxes)
  (define (distance one two)
    (+ (square (- (point-x one) (point-x two)))
       (square (- (point-y one) (point-y two)))
       (square (- (point-z one) (point-z two)))))

  (let loop ([boxes boxes]
             [connections '()])
    (if (null? boxes)
        (quick-sort-list connections (λ (x y) (< (car x) (car y))))
        (let ([one (car boxes)])
          (let inner ([rest (cdr boxes)]
                      [connections connections])
            (if (null? rest)
                (loop (cdr boxes) connections)
                (let* ([two (car rest)]
                       [dist (distance one two)])
                  (inner (cdr rest) (cons (cons dist (cons one two)) connections)))))))))

(define (parse port)
  (define (parse-number port)
    (let loop ([num 0])
      (if (char-digit? (peek-char port))
          (loop (+ (* 10 num) (char->number (read-char port))))
          num)))

  (define (expect-char port expected)
    (let ([char (read-char port)])
      (unless (char=? char expected)
        (error "expected char" expected "but got" char))))

  (let loop ([boxes '()])
    (let ([peek (peek-char port)])
      (if (eof-object? peek)
          (cons boxes (connections boxes))
          (begin
            (define x (parse-number port))
            (expect-char port #\,)
            (define y (parse-number port))
            (expect-char port #\,)
            (define z (parse-number port))
            (expect-char port #\newline)
            (loop (cons (point x y z) boxes)))))))

(define (hashset-intersects? one two)
  (> (hashset-length (hashset-intersection one two)) 0))

(define (connect-junction conn circuits)
  (let merge ([conn conn]
              [circuits circuits]
              [new '()])
    (cond
      [(null? circuits) (cons conn new)]
      [(hashset-intersects? (car circuits) conn)
       (merge (hashset-union (car circuits) conn) (cdr circuits) new)]
      [else (merge conn (cdr circuits) (cons (car circuits) new))])))

(define (one connections amt)
  (let loop ([connections (take connections amt)]
             [circuits '()])
    (if (null? connections)
        (apply * (take (quick-sort-list (map hashset-length circuits) >) 3))
        (let ([conn (hashset (cadar connections) (cddar connections))])
          (loop (cdr connections) (connect-junction conn circuits))))))

(define (two boxes connections)
  (let loop ([connections connections]
             [circuits (map hashset boxes)])
    (let* ([conn (hashset (cadar connections) (cddar connections))]
           [circuits (connect-junction conn circuits)])
      (if (null? (cdr circuits))
          (apply * (map point-x (hashset->list conn)))
          (loop (cdr connections) circuits)))))

(call-with-input-file "txt/day08.txt"
                      (λ (port)
                        (let* ([parsed (time! (parse port))]
                               [boxes (car parsed)]
                               [connections (cdr parsed)])
                          (displayln "--- one ---")
                          (displayln (time! (one connections 1000)))
                          (displayln "--- two ---")
                          (displayln (time! (two boxes connections))))))
