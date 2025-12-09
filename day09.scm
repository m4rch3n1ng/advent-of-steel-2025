(require "steel/time/time.scm")

(struct point (x y) #:transparent)
(struct line (x1 x2 y1 y2) #:transparent)

(define (make-line one two)
  (line (min (point-x one) (point-x two))
        (max (point-x one) (point-x two))
        (min (point-y one) (point-y two))
        (max (point-y one) (point-y two))))

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

  (let loop ([points '()]
             [lines '()])
    (let ([peek (peek-char port)])
      (if (eof-object? peek)
          (cons points (cons (make-line (car points) (last points)) lines))
          (begin
            (define x (parse-number port))
            (expect-char port #\,)
            (define y (parse-number port))
            (expect-char port #\newline)
            (define point (point x y))
            (if (null? points)
                (loop (cons point points) '())
                (loop (cons point points) (cons (make-line point (car points)) lines))))))))

(define (area one two)
  (* (+ (abs (- (point-x one) (point-x two))) 1) (+ (abs (- (point-y one) (point-y two))) 1)))

(define (one points)
  (let loop ([m 0]
             [points points]
             [rest points])
    (cond
      [(null? points) m]
      [(null? rest) (loop m (cdr points) (cdr points))]
      [else (loop (max m (area (car points) (car rest))) points (cdr rest))])))

(define (andmap pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (andmap pred (cdr lst))]
    [else #f]))

(define (rect-inside? one two lines)
  (define (line-outside? line)
    (or (<= (line-x2 line) (min (point-x one) (point-x two)))
        (>= (line-x1 line) (max (point-x one) (point-x two)))
        (<= (line-y2 line) (min (point-y one) (point-y two)))
        (>= (line-y1 line) (max (point-y one) (point-y two)))))

  (andmap line-outside? lines))

(define (two points lines)
  (let loop ([m 0]
             [points points]
             [rest points])
    (cond
      [(null? points) m]
      [(null? rest) (loop m (cdr points) (cdr points))]
      [else
       (let ([area (area (car points) (car rest))])
         (if (and (> area m) (rect-inside? (car points) (car rest) lines))
             (loop area points (cdr rest))
             (loop m points (cdr rest))))])))

(call-with-input-file "txt/day09.txt"
                      (λ (port)
                        (let* ([parsed (time! (parse port))]
                               [points (car parsed)]
                               [lines (cdr parsed)])
                          (displayln "--- one ---")
                          (displayln (time! (one points)))
                          (displayln "--- two ---")
                          (displayln (time! (two points lines))))))
