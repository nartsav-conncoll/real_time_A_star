(define path-lst '())

(define dist
  (lambda (pointa pointb)
    (+ (abs (- (car pointa) (car pointb))) (abs(- (cadr pointa) (cadr pointb))))))

(define heuristic
  (lambda (count pointa)
    (+ count (dist pointa goal))))

;(define compareHeuristic
;  (lambda (frontiera frontierb)
;    (< (heuristic (length (get-path frontiera)) frontiera) (heuristic (length (get-path frontierb)) frontierb))))

(define compareHeuristic
  (lambda (frontiera frontierb)
    (< (dist frontiera goal) (dist frontierb goal))))

(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (set! lst (list-sort compareHeuristic lst))
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