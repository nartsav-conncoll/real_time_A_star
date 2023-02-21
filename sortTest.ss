(define disp
  (lambda (input)
    (display input)
    (display "\n")))

(define heuristic
  (lambda (a b)
    (< (* a a) (* b b))))

(disp(list-sort heuristic '(3 4 2 1 2 5)))

(disp(list->string
  (list-sort equal?
    (string->list "hello"))))

(disp(length '((1 2) (3 4))))