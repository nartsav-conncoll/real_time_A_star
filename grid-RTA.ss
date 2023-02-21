(define path-lst '())
(define BFSpath-lst '())

;BFS search
(define BFSexpand 
  (lambda (point)
    (let ((lst (adjacentf point)));This doesn't work. How do we get BFS to only search on visited nodes
      (BFSadd-to-path-lst lst point)
      (enqueue lst))))

(define BFSadd-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! BFSpath-lst (cons child-parent BFSpath-lst))
         (BFSadd-to-path-lst (cdr lst) point)))))

(define BFSsearch
  (lambda (grid stop-count)
    ;(block-set! robot visited)
    (set! BFSpath-lst (list (list robot '())))
    (BFSsearch2 grid 1 stop-count)))

(define BFSsearch2
  (lambda (grid count stop-count)
    
    ;(display queue)
    (pause pause-num)
    ;(display count)
    ;(newline)
    ;(set! robot (dequeue))
    ;(draw-visited (car robot) (cadr robot))
    (let ((next-robot (dequeue)))
      (BFSexpand next-robot)
      (cond
        ; Stop when all the nodes in stack are in BFSpath-list. Use map and assoc
        (not (boolean? (assoc #f (map (lambda (frontier) (list (assoc frontier BFSpath-list))) stack))))
        ;We can add length of this getpath of the one we pick using length (pathBST)
        (else (BFSsearch2 grid (+ 1 count) stop-count))))))

(define BFSget-path
  (lambda (last-node)
    (cond
      ((equal? last-node robot) (list robot))
      (else (cons last-node (BFSget-path (cadr (assoc last-node BFSpath-lst))))))))

;A* search
(define dist
  (lambda (pointa pointb)
    (+ (abs (- (car pointa) (car pointb))) (abs(- (cadr pointa) (cadr pointb))))))

(define frontiercost
  (lambda (frontier)
    (length (BFSget-path frontier))))

(define heuristic
  (lambda (pointa)
    (+ (frontiercost pointa) (dist pointa goal))))
           
(define compareHeuristic
  (lambda (frontiera frontierb)
    (< (heuristic pointa) (heuristic pointb))))

(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (push lst))))
           
(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst (cons child-parent path-lst))
         (add-to-path-lst (cdr lst) point)))))

(define set-lst-visited 
  (lambda (lst)
    (if (null? lst)
        '()
    ;else
        (let ((x (car lst)))
          (draw-pt-frontier x)
          (block-set! x visited)
          (set-lst-visited (cdr lst))))))
  
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

(define search
  (lambda (grid stop-count)
    (block-set! start visited)
    (set! path-lst (list (list start '())))
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    ;(display stack)
    (pause pause-num)
    ;(display count)
    (newline)
    (expand robot)
    (set! queue '())
    (BFSsearch grid stop-count)
    (set! lst (list-sort compareHeuristic lst))
    (set! robot (pop))
    (draw-visited (car robot) (cadr robot))
    (cond
      ((equal? robot goal) (draw-path (get-path robot)) (get-path robot))
      (else (search2 grid (+ 1 count) stop-count)))))

(define get-path
  (lambda (last-node)
    (cond
      ((equal? last-node start) (list start))
      (else (cons last-node (get-path (cadr (assoc last-node path-lst))))))))

      
(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
 
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))