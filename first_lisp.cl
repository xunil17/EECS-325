(in-package :cs325-user)
;;;notes: cond syntax, if its empty, returns null, listp returns if something is a list, else is represented by t


;;; exercise 4

(defun greater (a b)
  (cond 
   ((> a b) a)
   (t b)))

;;; use the cond statement to convert the > operator to return a value

;;;exercise 7
;;; take a list as an argument and return true if one of its element is a list

(defun has-list-p (a)
  (cond
   ((null a) NIL)
   ((listp (car a)) t)
   (t (has-list-p (cdr a)))))
  

;;;exercise 8 part 1 a
;;; recursive version
#|
(defun print-dots (n)
  (cond 
   ((= n 0) nil)
   (t (format t ".")
      (print-dots (1- n))))) |#


;;;exercise 8 part 2 a
(defun print-dots (x)
  (do ((i 0 ( 1+ i)))
      ((>= i x))
    (format t ".")))

;;;exercise 8 part 1 b recursive

#|(defun get-a-count (x)
  (cond
   ((null x) 0)
   ((eql (car x) 'a) (1+ (get-a-count (cdr x))))
   (t (get-a-count (cdr x))))) |#


;;;exercise 8 part 2 b
;;; nonrecursive

#|(defun get-a-count (x)
  (let ((count 0))
    (dolist (obj x)
      (when (eql obj 'a) (incf count)))
      )
  count) |#

(defun get-a-count (x)
  (do ((xnew x (cdr xnew))
       (i 0 (+ i (cond 
                  ((eql 'a (car xnew)) 1)
                  (t 0)))))
      ((null xnew) i)))

;;;exercise 9
#|(defun summit (lst)
  (apply #'+ (remove nil lst)))|#


;;;you can't do summit cdr lst because it'll return nil which creates an infinite loop
#|(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
        (summit (cdr lst))
      (+ x (summit (cdr lst))))))|#



#|(defun summit (lst)
   (if lst
      (+ (or (car lst) 0) (summit (cdr lst)))
     0))|#
      

(defun summit (lst)
  (if (null lst) 0
  (let ((x (car lst)))
    (cond
     ((null x) (summit (cdr lst)))
     (t (+ x (summit (cdr lst))))))))

#|(defun summit (lst)
  (let ((x (car lst)))
    (cond
     ((not lst) 0)
     ((null x) (summit (cdr lst)))
     (t (+ x (summit (cdr lst)))))))|#

