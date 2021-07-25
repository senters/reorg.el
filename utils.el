(defun filter (f coll)
  (delete nil (mapcar (lambda (el) (if (funcall f el) el nil)) coll)))
;; example:
;; (filter (lambda (arg) (> (/ arg 50) 0.04)) '(2.0 4.0 6.0))

(defun first-match (f coll) (car (filter f coll)))
;; example:
;; (first-match (lambda (arg) (> (/ arg 50) 0.04)) '(2.0 4.0 6.0))

(defun vector->list (v)
  (append v nil))

(defun current-col-space ()
  (apply 'concat (make-list (current-column) " ")))

(defmacro dovector (arg-list &rest forms)
  `(dolist (,(car arg-list) (vector->list ,(cadr arg-list)))
     ,@forms))

(defun prn-scratch (data)
  (print data (get-buffer "*scratch*")))

(defun mapcaar-dotpair-list (fn l)
  (mapcar (lambda (pair) (cons (funcall fn (car pair)) (cdr pair))) l))
