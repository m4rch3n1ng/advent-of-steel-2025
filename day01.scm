(require "steel/time/time.scm")

(define (parse port)
  (let ([input (read-port-to-string port)])
    (map (λ (lst)
           (let ([num (string->number (list->string (cdr lst)))])
             (if (char=? #\L (car lst))
                 (- num)
                 num)))
         (map string->list (filter (λ (s) (> (string-length s) 0)) (split-many input "\n"))))))

(define (one lst)
  (let loop ([lst lst]
             [acc 50]
             [zeroes 0])
    (cond
      [(and (null? lst) (zero? acc)) (+ zeroes 1)]
      [(null? lst) zeroes]
      [(zero? acc) (loop (cdr lst) (euclidean-remainder (+ acc (car lst)) 100) (+ 1 zeroes))]
      [else (loop (cdr lst) (euclidean-remainder (+ acc (car lst)) 100) zeroes)])))

(define (two lst)
  (let loop ([lst lst]
             [acc 50]
             [zeroes 0])

    (if (null? lst)
        zeroes
        (let* ([new-acc (+ acc (car lst))]
               [new-zeroes (abs (truncate-quotient new-acc 100))])
          (loop (cdr lst)
                (euclidean-remainder new-acc 100)
                (+ new-zeroes
                   (if (or (zero? new-acc) (and (negative? new-acc) (not (zero? acc)))) 1 0)
                   zeroes))))))

(call-with-input-file "txt/day01.txt"
                      (λ (port)
                        (let ([lst (time! (parse port))])
                          (displayln "--- one ---")
                          (displayln (time! (one lst)))
                          (displayln "--- two ---")
                          (displayln (time! (two lst))))))
