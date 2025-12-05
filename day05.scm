(require "steel/time/time.scm")

(define (parse port)
  (cons (let loop ([lst '()]
                   [prev #f]
                   [curr 0])
          (let ([char (read-char port)])
            (cond
              [(char=? char #\newline)
               (if prev
                   (loop (cons (cons prev curr) lst) #f 0)
                   lst)]
              [(char=? char #\-) (loop lst curr 0)]
              [else (loop lst prev (+ (* curr 10) (char->number char)))])))
        (let loop ([lst '()]
                   [num 0])
          (let ([char (read-char port)])
            (cond
              [(eof-object? char) lst]
              [(char=? char #\newline) (loop (cons num lst) 0)]
              [else (loop lst (+ (* num 10) (char->number char)))])))))

(define (ormap pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (ormap pred (cdr lst))]))

(define (fresh? ranges ingredient)
  (ormap (λ (range) (and (<= ingredient (cdr range)) (>= ingredient (car range)))) ranges))

(define (one ranges ingredients)
  (length (filter (λ (ingredient) (fresh? ranges ingredient)) ingredients)))

(define ((overlaps? two) one)
  (or (and (>= (car one) (car two)) (<= (car one) (cdr two)))
      (and (>= (cdr one) (car two)) (<= (cdr one) (cdr two)))
      (and (<= (car one) (car two)) (>= (cdr one) (cdr two)))
      (and (<= (car two) (car one)) (>= (cdr two) (cdr one)))))

(define (merge ranges)
  (define (merge-two one two)
    (cons (min (car one) (car two)) (max (cdr one) (cdr two))))

  (let loop ([range (car ranges)]
             [ranges (cdr ranges)]
             [lst '()])
    (if (null? ranges)
        (cons range lst)
        (let* ([overlaps? (overlaps? range)]
               [overlapping (filter overlaps? ranges)])
          (if (null? overlapping)
              (loop (car ranges) (cdr ranges) (cons range lst))
              (loop (foldl (λ (x y) (merge-two x y)) range overlapping)
                    (filter (λ (x) (not (overlaps? x))) ranges)
                    lst))))))

(define (two ranges)
  (let ([ranges (merge ranges)])
    (apply + (map (λ (range) (+ (- (cdr range) (car range)) 1)) ranges))))

(call-with-input-file "txt/day05.txt"
                      (λ (port)
                        (let* ([tmp (time! (parse port))]
                               [ranges (car tmp)]
                               [ingredients (cdr tmp)])
                          (displayln "--- one ---")
                          (displayln (time! (one ranges ingredients)))
                          (displayln "--- two ---")
                          (displayln (time! (two ranges))))))
