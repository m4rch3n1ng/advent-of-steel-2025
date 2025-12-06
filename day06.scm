(require "steel/time/time.scm")

(define (parse port)
  (define (parse-operators text)
    (define port (open-input-string text))
    (let loop ([lst '()]
               [op #f])
      (let ([char (read-char port)])
        (cond
          [(eof-object? char) (reverse (cons (reverse op) lst))]
          [(or (char=? char #\+) (char=? char #\*))
           (if op
               (loop (cons (reverse op) lst) (cons char '()))
               (loop lst (cons char '())))]
          [(char=? char #\space) (loop lst (cons char op))]
          [else (error "unexpected char" char)]))))

  (let* ([text (read-port-to-string port)]
         [lines (split-many text "\n")]
         [lines (cdr (reverse lines))]
         [operators (parse-operators (car lines))]
         [numbers (cdr lines)])
    (let loop ([lines numbers]
               [acc (map (λ (op)
                           (cond
                             [(char=? (car op) #\+) (list +)]
                             [else (list *)]))
                         (reverse operators))])
      (if (null? lines)
          (map reverse acc)
          (let inner ([ops operators]
                      [line (string->list (car lines))]
                      [lst '()])
            (if (null? line)
                (loop (cdr lines) (map cons lst acc))
                (let ([len (length (car ops))])
                  (inner (cdr ops) (drop line len) (cons (take line len) lst)))))))))

(define (one sheet)
  (apply +
         (map (λ (problem)
                (apply (car problem)
                       (map string->number (map trim (map list->string (cdr problem))))))
              sheet)))

(define (two sheet)
  (apply +
         (map (λ (problem)
                (apply (car problem)
                       (map string->number
                            (filter (λ (str) (> (string-length str) 0))
                                    (map trim
                                         (map list->string
                                              (map reverse
                                                   (apply map (cons list (cdr problem))))))))))
              sheet)))

(call-with-input-file "txt/day06.txt"
                      (λ (port)
                        (let ([sheet (time! (parse port))])
                          (displayln "--- one --")
                          (println (time! (one sheet)))
                          (displayln "--- two ---")
                          (println (time! (two sheet))))))
