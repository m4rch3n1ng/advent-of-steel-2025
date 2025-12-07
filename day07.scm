(require "steel/time/time.scm")

(define (parse port)
  (let loop ([x 0]
             [y 0]
             [set (hashset)]
             [beam #f])
    (let ([char (read-char port)])
      (cond
        [(eof-object? char) (cons set (cons beam y))]
        [(char=? char #\newline) (loop 0 (+ y 1) set beam)]
        [(char=? char #\.) (loop (+ x 1) y set beam)]
        [(char=? char #\S) (loop (+ x 1) y set (cons x y))]
        [(char=? char #\^) (loop (+ x 1) y (hashset-insert set (cons x y)) beam)]
        [else (error "unexpected char" char)]))))

(define (move pos x y)
  (cons (+ (car pos) x) (+ (cdr pos) y)))

(define (one splitter beam height)
  (let loop ([idx height]
             [beams (list beam)]
             [splits 0])
    (if (= idx 0)
        splits
        (let inner ([beams beams]
                    [new '()]
                    [s 0])
          (if (null? beams)
              (loop (- idx 1) (hashset->list (list->hashset new)) (+ splits s))
              (let* ([pos (move (car beams) 0 1)])
                (if (hashset-contains? splitter pos)
                    (inner (cdr beams) (cons (move pos -1 0) (cons (move pos 1 0) new)) (+ s 1))
                    (inner (cdr beams) (cons pos new) s))))))))

(define (hash->list hm)
  (map (λ (x) (cons (car x) (cadr x))) (transduce hm (into-list))))

(define (two splitter beam height)
  (define (dedupe lst)
    (let loop ([lst lst]
               [hm (hash)])
      (if (null? lst)
          (hash->list hm)
          (let* ([item (car lst)]
                 [pos (car item)]
                 [amt (cdr item)])
            (cond
              [(hash-contains? hm pos)
               (loop (cdr lst) (hash-insert hm pos (+ amt (hash-ref hm pos))))]
              [else (loop (cdr lst) (hash-insert hm pos amt))])))))

  (let loop ([idx height]
             [beams (list (cons beam 1))])
    (if (= idx 0)
        (apply + (map cdr beams))
        (let inner ([beams beams]
                    [new '()])
          (if (null? beams)
              (loop (- idx 1) (dedupe new))
              (let* ([curr (car beams)]
                     [pos (move (car curr) 0 1)]
                     [amt (cdr curr)])
                (if (hashset-contains? splitter pos)
                    (inner (cdr beams)
                           (cons (cons (move pos -1 0) amt) (cons (cons (move pos 1 0) amt) new)))
                    (inner (cdr beams) (cons (cons pos amt) new)))))))))

(call-with-input-file "txt/day07.txt"
                      (λ (port)
                        (let* ([tmp (time! (parse port))]
                               [splitter (car tmp)]
                               [beam (cadr tmp)]
                               [height (cddr tmp)])
                          (displayln "--- one ---")
                          (displayln (time! (one splitter beam height)))
                          (displayln "--- two ---")
                          (displayln (time! (two splitter beam height))))))
