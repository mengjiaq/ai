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
(define (is-goal? v)
  (or (equal? v 'r123)
      (equal? v 'storage)
      (equal? v 'd1)))
(define goals (list 'r123 'storage 'd1))

;; helper function: add addon to all element in list of list
;; Datatype:
;;   addon: node
;;   lst: list of list of node
;; return : list of list of nodes
(define (addAll addon lst)
  (map (lambda (x) (cons addon x)) lst))

;;dfs
(define (mydfs graph start used)
  (if (member? start goals) (list (list start))
      (addAll start (dfs-helper1 '() graph (cons start used) (get-neighbors graph start)))
        ))

(define (dfs-helper1 all-solutions graph used nbrs)
  (cond [(empty? nbrs) all-solutions]
        [(member? (first nbrs) used) (dfs-helper1 all-solutions graph used (rest nbrs))]
        [else (append (dfs-helper1 all-solutions graph used (rest nbrs)) (mydfs graph (first nbrs) used))]))
         ;; start

;;bfs
(define (mybfs queue graph start lst)
  (cond [(and (empty? queue) ;; if queue is empty and start is null(not the init-state), return result      
              (null? start)) 
         (reverse lst)] ;;(use reverse because cons puts element to the front, causing the result to reversed order)
        [(empty? queue) ;; if queue is empty (and start not null) --> init-state, put start into queue
         (mybfs (list (list start)) graph null lst)]
        [(is-goal? (first (first queue))) ;; if a goal reached
         (mybfs (rest queue) graph start (cons (reverse (first queue)) lst))];; add the path into lst, continue in the queue
        [else (mybfs (append (rest queue) ;; go one step further on the path and enqueue them
                             (bfs-helper graph '() (first queue) (get-neighbors graph (first (first queue))))) graph null lst)
                      ]))

(define (bfs-helper graph paths path nbrs)
  (cond [(empty? nbrs) paths]
        [(member? (first nbrs) path) (bfs-helper graph paths path (rest nbrs))];; there is a loop in the path if add the nbr
        [else (bfs-helper graph (cons (cons (first nbrs) path) paths) path (rest nbrs))]))


;;lowest-cost

(define (my-lowest-cost queue graph start lst)
  (cond [(and (empty? queue) ;; if queue is empty and start is null(not the init-state), return result      
              (null? start)) 
         (sort (reverse lst) compare)] ;;(use reverse because cons puts element to the front, causing the result to reversed order)
        [(empty? queue) ;; if queue is empty (and start not null) --> init-state, put start into queue
         (my-lowest-cost (list (list start)) graph null lst)]
        [(is-goal? (first (first queue))) (my-lowest-cost (rest queue) graph start (cons (reverse (first queue)) lst))]
        [else (my-lowest-cost (sort ;; sort queue according to acsending order of cost sofar
                               (append (rest queue) (bfs-helper graph '() (first queue) (get-neighbors graph (first (first queue)))))
                                    compare) graph null lst)
              
                      ]))
;; comparator for sorting by cost
(define compare
  (lambda (x y) (< (acc x 0) (acc y 0))))
;; accumulate cost along path
;; lst: path (list of node)
;; result: Int  so-far cost
;; return: Int  total cost
(define (acc lst result)
  (if (= 1 (length lst)) result
      (acc (rest lst) (+ result (edge-weight g (first lst) (second lst))))))

;; best-first (heuristics: manhattan distance)
;; the straight-line distance between the node and its closest goal can be used as the heuristic function
;; based on bfs, everytime when choosing the next neighbor to be added into the path, sort them first according to their manhattan distance
(define (my-best-first queue graph start lst)
  (cond [(and (empty? queue) ;; if queue is empty and start is null(not the init-state), return result      
              (null? start)) 
         (sort (reverse lst) compare)] ;;(use reverse because cons puts element to the front, causing the result to reversed order)
        [(empty? queue) ;; if queue is empty (and start not null) --> init-state, put start into queue
         (my-best-first (list (list start)) graph null lst)]
        [(is-goal? (first (first queue))) (my-best-first (rest queue) graph start (cons (reverse (first queue)) lst))]
        [else (my-best-first (append (rest queue) (bfs-helper graph '() (first queue)
                                                              (sort (get-neighbors graph (first (first queue))) compare-h)))
                             ;;sort neighbor according to acsending order of manhattan distance
                                     graph null lst)
                      ]))

(define (pos node) ;; positions for each spot
  (cond [(eq? 'r123 node) (list 0 4)]
        [(eq? 'o125 node) (list 1 3)]
        [(eq? 'o123 node) (list 1 4)]
        [(eq? 'o119 node) (list 1 6)]
        [(eq? 'storage node) (list 1 7)]
        [(eq? 'o125 node) (list 1 3)]
        [(eq? 'd1 node) (list 2 1)]
        [(eq? 'd2 node) (list 2 3)]
        [(eq? 'c1 node) (list 2 4)]
        [(eq? 'd3 node) (list 3 3)]
        [(eq? 'c2 node) (list 3 4)]
        [(eq? 'c3 node) (list 3 5)]
        [(eq? 'a1 node) (list 3 3)]
        [(eq? 'b1 node) (list 3 4)]
        [(eq? 'b2 node) (list 3 5)]
        [(eq? 'a2 node) (list 4 1)]
        [(eq? 'a3 node) (list 4 3)]
        [(eq? 'b3 node) (list 4 4)]
        [(eq? 'b4 node) (list 4 5)]
        [(eq? 'mail node) (list 5 0)]
        [(eq? 'ts node) (list 5 1)]
        [(eq? 'o103 node) (list 5 4)]
        [(eq? 'o109 node) (list 5 7)]
        [(eq? 'o111 node) (list 5 8)]))

(define (manhattan-distance a b) ;; manhattan distance from a to b
  (+ (abs (- (first (pos a)) (first (pos b))))
     (abs (- (second (pos a)) (second (pos b))))))

(define (heuristic a goals) ;; heuristics: minimum manhattan distance to any goal
  (if (empty? goals) +inf.0
      (min (manhattan-distance a (first goals))
           (heuristic a (rest goals)))))
;; comparator: compare two nodes' heuristic value
(define compare-h
  (lambda (a b) (< (heuristic a goals) (heuristic b goals))))
;; dls: deapth-limited-search

(define (mydls graph start limit used)
  (cond [(>= 0 limit) (list)]
        [(is-goal? start) (list (list start))]
        [else (addAll start (dfs-helper2 '() graph  (get-neighbors graph start) limit (cons start used)))]))

(define (dfs-helper2 all-solutions graph nbrs limit used)
  (cond [(empty? nbrs) all-solutions]
        [(member? (first nbrs) used) (dfs-helper2 all-solutions graph (rest nbrs) limit used)]
        [else (append (dfs-helper2 all-solutions graph (rest nbrs) limit used) (mydls graph (first nbrs) (- limit 1) used))]))
         ;; start
(define (my-astar queue graph start lst)
  (cond [(and (empty? queue) ;; if queue is empty and start is null(not the init-state), return result      
              (null? start)) 
         (sort (reverse lst) compare)] ;;(use reverse because cons puts element to the front, causing the result to reversed order)
        [(empty? queue) ;; if queue is empty (and start not null) --> init-state, put start into queue
         (my-astar (list (list start)) graph null lst)]
        [(is-goal? (first (first queue))) (my-astar (rest queue) graph start (cons (reverse (first queue)) lst))]
        [else (my-astar (append (rest queue) (bfs-helper graph '() (first queue)
                                                              (sort (get-neighbors graph (first (first queue)))
                                                                    (compare-astar graph (first (first queue))))))
                             ;;sort neighbor according to acsending order of manhattan distance
                                     graph null lst)
                      ]))
(define (compare-astar graph prev)
  (lambda (a b) (< (+ (edge-weight graph prev a) (heuristic a goals))
                   (+ (edge-weight graph prev b) (heuristic b goals)))))
(define-struct pair (prev cur))
;;PART3:
;;my bidirection bfs
(define (mybi-bfs graph start goal)
  (mybi-bfs-helper start goal (list start) (list goal) graph '((list null start) (list null goal))))
;; helper:
;; INPUT:
;;  start: start point;
;;  goal: goal point
;;  startset: the set of nodes represents the frontier from start
;;  goalset the set of nodes represents the frontier from goal
;;  used: set of pairs of nodes visited before (prev, cur)
;; Return: only one path (the shortest one)
(define (mybi-bfs-helper start goal startset goalset graph used)
  (let ((node (interact startset goalset))
        (nextstart (nextfrontier startset (list (list) (list)) used graph))
        ;; nextstart: (list (frontier) (used)) consists of two parts:
        ;;       first part, frontier of the next step;
        ;;       second part: used pairs between current frontier and next frontier
        )
  (cond [(or (= 0 (length startset))
            (= 0 (length goalset)))
            '()]
        [(< (length startset)
            (length goalset));; always expand the larger one (if startset < goalset, swap those two)
            (mybi-bfs-helper goal start goalset startset graph used)]
        [(null? node);; if no intersaction, continue to expand
         (mybi-bfs-helper start goal (first nextstart) goalset
                                       graph (append (second nextstart) used))]
        [else (append  (reconstruct used node start '())
                       (rest (reverse (reconstruct used node goal '())))
                       )])))
;;
(define (nextfrontier frontier next used graph)
  (if (empty? frontier) next
      (let ((cur (first frontier))
            (nbrs (get-neighbors graph (first frontier)))
            (emptylst (list (list) (list))))
          (nextfrontier (rest frontier)
                    (list (append (first next) (first (getnext nbrs emptylst used cur)))
                          (append (second next) (second (getnext nbrs emptylst used cur))))
                    (append used (second (getnext nbrs emptylst used cur))) graph))))

(define (getnext nbrs lst used cur)
  (let ((newfrontier (first lst))
        (newused (second lst)))
  (cond [(empty? nbrs) lst]
        [(null? (find-prev (first nbrs) used))
         (getnext (rest nbrs) (list (cons (first nbrs) newfrontier) (cons (list cur (first nbrs)) newused)) used cur)]
        [else (getnext (rest nbrs) lst used cur)])))
;; find the previous node in path
;; node: the current node
;; used: set of pairs of nodes visited before (prev, cur)
(define (find-prev node used)
  (cond [(empty? used) null]
        [(equal? node (second (first used))) (first (first used))]
        [else (find-prev node (rest used))]))
;; look for interaction of two frontiers
;; return: either null if no interaction or the node in both sets
(define (interact startset goalset)
  (cond [(empty? goalset) null]
        [(member? (first goalset) startset) (first goalset)]
        [else (interact startset (rest goalset))]))
;; reconstruct path towards two endpoints
(define (reconstruct used node goal path)
  (if (equal? node goal) (cons goal path)
      (reconstruct used (find-prev node used) goal (cons node path))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAIN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"using dfs"
(mydfs g 'o103 '())
"using bfs"
(mybfs '() g 'o103 '())
"using lowest-cost"
(my-lowest-cost '() g 'o103 '())
"using best-first"
(my-best-first '() g 'o103 '())
"using dls"
(mydls g 'o103 12 '())
"using a*"
(my-astar '() g 'o103 '())
"using bidirectional search"
(mybi-bfs g 'r123 'o123)