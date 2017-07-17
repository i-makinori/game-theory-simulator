(in-package #:game-theory-simulator)

;;;; list-function
(defun map-matrix (func matrix)
  (mapcar #'(lambda (list)
	      (mapcar #'(lambda (x)  (funcall func x)) 
		      list))
	  matrix))

(defun x-to-y-list (x y)
  (let* ((func (if (< x y) #'1+ #'1-))
	 (num-now (+ x (if (< x y) -1 1))))
    (mapcar
     #'(lambda (n) n
	 (setf num-now (funcall func num-now))
	 num-now)
     (make-list (1+ (abs (- x y)))))))

(defun init (list)
  (reverse (cdr (reverse list))))

(defun roll-list (x list)
  (cons x (init list)))

(defun flatten (orig-list)
  (if (eql orig-list nil)
      nil
      (let ((elem (car orig-list)) (resto-list (cdr orig-list)))
	(if (listp elem)
	    (append (flatten elem) (flatten resto-list))
	    (append (cons elem nil) (flatten resto-list))))))


;;;; array-function
(defun array-map (function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (flet ((make-displaced-array (array)
           (make-array (reduce #'* (array-dimensions array))
                       :displaced-to array)))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays))
           (result-array (make-array (array-dimensions (first arrays))))
           (displaced-result-array (make-displaced-array result-array)))
      (declare (dynamic-extent displaced-arrays displaced-result-array))
      (apply #'map-into displaced-result-array function displaced-arrays)
      result-array)))

(defun show-board (board)
  (loop for i below (car (array-dimensions board)) do
       (loop for j below (cadr (array-dimensions board)) do
	    (let ((cell (aref board i j)))
	      (format t "~a " cell)))
       (format t "~%")))


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun coordinate-matrix (width height)
  (list-to-2d-array ;; coordinate-matrix
   (mapcar #'(lambda (y)
	       (mapcar #'(lambda(x) (list y x))
		       (x-to-y-list 0 width)))
	   (x-to-y-list 0 height))))
