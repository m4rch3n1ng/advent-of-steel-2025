(require "steel/time/time.scm")

(define (parse port)
  (let loop ([lst '()]
             [bank '()])
    (let ([char (read-char port)])
      (cond
        [(eof-object? char) lst]
        [(char=? char #\newline) (loop (cons (reverse bank) lst) '())]
        [else (loop lst (cons (char->number char) bank))]))))

(define (largest-joltage bank)
  (let loop ([first (car bank)]
             [second (cadr bank)]
             [rest (cdr bank)])
    (cond
      [(null? (cdr rest)) (+ (* first 10) second)]
      [(> (car rest) first) (loop (car rest) (cadr rest) (cdr rest))]
      [(> (cadr rest) second) (loop first (cadr rest) (cdr rest))]
      [else (loop first second (cdr rest))])))

(define (one lst)
  (apply + (map largest-joltage lst)))

(define (max-index lst)
  (let loop ([curr (cons (car lst) 0)]
             [idx 1]
             [lst (cdr lst)])
    (cond
      [(null? lst) curr]
      [(> (car lst) (car curr)) (loop (cons (car lst) idx) (+ idx 1) (cdr lst))]
      [else (loop curr (+ idx 1) (cdr lst))])))

(define (largest-joltage-12 lst)
  (let loop ([lst lst]
             [idx 12]
             [acc 0])
    (if (= idx 0)
        acc
        (let* ([len (length lst)]
               [tmp (take lst (- len (- idx 1)))]
               [max (max-index tmp)])
          (loop (drop lst (+ (cdr max) 1)) (- idx 1) (+ (* acc 10) (car max)))))))

(define (two lst)
  (apply + (map largest-joltage-12 lst)))

(call-with-input-file "txt/day03.txt"
                      (λ (port)
                        (let ([lst (time! (parse port))])
                          (displayln "--- one ---")
                          (displayln (time! (one lst)))
                          (displayln "--- two ---")
                          (displayln (time! (two lst))))))
