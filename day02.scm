(require "steel/time/time.scm")

(struct range (start end) #:transparent)

(define (parse port)
  (let loop ([lst '()]
             [buf (open-output-string)]
             [last #f])
    (let ([char (read-char port)])
      (cond
        [(char-digit? char)
         (write-char char buf)
         (loop lst buf last)]
        [(char=? char #\-) (loop lst (open-output-string) (string->number (get-output-string buf)))]
        [(char=? char #\,)
         (loop (cons (range last (string->number (get-output-string buf))) lst)
               (open-output-string)
               #f)]
        [(char=? char #\newline) (cons (range last (string->number (get-output-string buf))) lst)]))))

(define (iter lst invalid?)
  (let outer ([invalid 0]
              [lst lst])
    (if (null? lst)
        invalid
        (begin
          (define r (car lst))
          (let inner ([i 0]
                      [num (range-start r)])
            (cond
              [(> num (range-end r)) (outer (+ i invalid) (cdr lst))]
              [(invalid? num) (inner (+ num i) (+ num 1))]
              [else (inner i (+ num 1))]))))))

(define (invalid? num)
  (define width (+ (exact (floor (log num 10))) 1))
  (if (even? width)
      (begin
        (define-values (l r) (truncate/ num (expt 10 (/ width 2))))
        (= l r))
      #f))

(define (one lst)
  (iter lst invalid?))

(define (more-invalid? num)
  (define width (+ (exact (floor (log num 10))) 1))
  (define (split div)
    (define exp (expt 10 div))
    (define-values (rest curr) (truncate/ num exp))
    (let loop ([left (- width div)]
               [curr curr]
               [rest rest])
      (if (= left div)
          (= curr rest)
          (begin
            (define-values (rest next) (truncate/ rest exp))
            (if (= curr next)
                (loop (- left div) next rest)
                #f)))))
  (let loop ([i 1])
    (cond
      [(> i (/ width 2)) #f]
      [(= (remainder width i) 0)
       (if (split i)
           #t
           (loop (+ i 1)))]
      [else (loop (+ i 1))])))

(define (two lst)
  (iter lst more-invalid?))

(call-with-input-file "txt/day02.txt"
                      (λ (port)
                        (let ([lst (time! (parse port))])
                          (displayln "--- one ---")
                          (displayln (time! (one lst)))
                          (displayln "--- two ---")
                          (displayln (time! (two lst))))))
