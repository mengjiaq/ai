;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assign1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require graph)
(define g (weighted-graph/directed '((6 ts mail)
                                     (6 mail ts)
                                     (8 o103 ts) (8 ts o103)
                                     (4 o103 b3)
                                     (12 o103 o109) (12 o109 o103)
                                     (16 o109 o119) (16 o119 o109)
                                     (4 o109 o111) (4 o111 o109)
                                     (3 b1 c2)
                                     (6 b1 b2) (6 b2 b1)
                                     (3 b2 b4) (3 b4 b2)
                                     (4 b3 b1) (4 b1 b3)
                                     (7 b3 b4) (7 b4 b3)
                                     (7 b4 o109)
                                     (8 c1 c3) (8 c3 c1)
                                     (6 c2 c3) (6 c3 c2)
                                     (4 c2 c1) (4 c1 c2)
                                     (4 d2 d3) (4 d3 d2)
                                     (8 d1 d3) (8 d3 d1)
                                     (2 o125 d2) (2 d2 o125)
                                     (4 o123 o125) (4 o125 o123)
                                     (4 o123 r123) (4 r123 o123)
                                     (9 o119 o123) (9 o123 o119)
                                     (7 o119 storage) (7 storage o119))))

(define (mydfs graph start used)
  (dfs-helper '() graph start used))

(define (dfs-helper all-solutions graph start used)
  (if (is-goal? start) (cons (list start) all-solutions)
      (addAll start (helper all-solutions graph (cons start used) (get-neighbors graph start)))
        ))

(define (helper all-solutions graph used nbrs)
  (cond [(empty? nbrs) all-solutions]
        [(member? (first nbrs) used) (helper all-solutions graph used (rest nbrs))]
        [else (append (helper all-solutions graph used (rest nbrs)) (mydfs graph (first nbrs) used))]))
         ;; start
(define (addAll addon lst)
  (map (lambda (x) (cons addon x)) lst))
(define (is-goal? v)
  (or (equal? v 'r123)
      (equal? v 'storage)
      (equal? v 'd1)))


(define (acc lst result)
  (if (= 1 (length lst)) result
      (acc (rest lst) (+ result (edge-weight g (first lst) (second lst))))))

(define (my-best-first graph start used)
  (sort (mydfs graph start used) (lambda (x y) (< (acc x 0) (acc y 0)))))
"using dfs"
(mydfs g 'o103 '())

"using best-first"
(my-best-first g 'o103 '())