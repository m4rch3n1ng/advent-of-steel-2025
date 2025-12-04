(require "steel/time/time.scm")

(define (parse port)
  (let loop ([pos (cons 0 0)]
             [set (hashset)])
    (let ([char (read-char port)])
      (cond
        [(eof-object? char) set]
        [(char=? char #\.) (loop (cons (+ (car pos) 1) (cdr pos)) set)]
        [(char=? char #\@) (loop (cons (+ (car pos) 1) (cdr pos)) (hashset-insert set pos))]
        [(char=? char #\newline) (loop (cons 0 (+ (cdr pos) 1)) set)]))))

(define (adjacent pos x y)
  (cons (+ (car pos) x) (+ (cdr pos) y)))

; this procedure assumes the given position is in the set
(define (accessible? set pos)
  (let loop ([lst (list (adjacent pos 1 0)
                        (adjacent pos 1 1)
                        (adjacent pos 0 1)
                        (adjacent pos -1 1)
                        (adjacent pos -1 0)
                        (adjacent pos -1 -1)
                        (adjacent pos 0 -1)
                        (adjacent pos 1 -1))]
             [amt 0])
    (cond
      [(null? lst) (< amt 4)]
      [(hashset-contains? set (car lst)) (loop (cdr lst) (+ amt 1))]
      [else (loop (cdr lst) amt)])))

(define (all-accessible set)
  (filter (λ (pos) (accessible? set pos)) (hashset->list set)))

(define (one set)
  (length (all-accessible set)))

(define (two set)
  (let loop ([set set]
             [acc 0])
    (let ([accessible (all-accessible set)])
      (if (null? accessible)
          acc
          (loop (hashset-difference set (list->hashset accessible)) (+ acc (length accessible)))))))

(call-with-input-file "txt/day04.txt"
                      (λ (port)
                        (let ([set (time! (parse port))])
                          (displayln "--- one ---")
                          (displayln (time! (one set)))
                          (displayln "--- two ---")
                          (displayln (time! (two set))))))
