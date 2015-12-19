;;;; data-base : field-data 

(defparameter *betrayal* 'd)
(defparameter *cooperation* 'c)
(defparameter *none* 'n)


(defparameter *field-width* 10)
(defparameter *field-height* 10)
(defparameter *field-energy* 160)
(defparameter *field-latch-energy* 3)
(defparameter *field-each-storage* 5)

;; data-base : strategy
(defparameter *all-a-func* #'(lambda (list) list *cooperation*))
(defparameter *all-d-func* #'(lambda (list) list *betrayal*))
(defparameter *tft-func*   #'(lambda (list) (if (eq (car list) *cooperation*)
						*cooperation* *betrayal*)))
(defparameter *all-a-flag* 'all-a)
(defparameter *all-d-flag* 'all-d)
(defparameter *tft-flag*   'tft)

(defparameter strategy-func-flag-catalog
  (mapcar #'list
	  (list *all-a-func* *all-d-func* *tft-func*)
	  (list *all-a-flag* *all-d-flag* *tft-flag*)))

;; to-init-plant
(defparameter *energy-par-plant* (/ *field-energy* (* *field-width* *field-height*)))
(defparameter *first-plant-storage*
  (make-list 4
	     :initial-element (make-list *field-each-storage*
					 :initial-element *cooperation*)))
(defparameter *first-plant-execut* (make-list 4 :initial-element *cooperation*))

(defun get-random-str-func-flag ()
  (nth (random 3) strategy-func-flag-catalog))

;;;; structure
(defstruct plant
  (strategy-storage '(())) ;; upper right lower left
  (strategy-executed '())
  (strategy-func #'(lambda (lis) lis 'nil))
  (strategy-flag 'nil)
  (energy 0))

(defstruct field
  (width 0)
  (height 0)
  (plants-array #2a(()))
  (passed-turn 0)
  (exist-energy 0)
  (latch-energy 0)
  (each-plants-storage-size 0))


;;;; field ;;;;;;;;;;;;;;;;;;;;

;; init-plant ;;;;;;;;;
(defun init-plant (&key (strategy-func nil) (strategy-flag nil)
		     (energy-par-plant *energy-par-plant*)
		     (strategy-storage *first-plant-storage*)
		     (first-execut *first-plant-execut*))
  (let ((str-func-flag (if (or (null strategy-func) (null strategy-flag))
			   (get-random-str-func-flag)
			   (list strategy-func strategy-flag))))
    (make-plant :strategy-func (car str-func-flag)
		:strategy-flag (cadr str-func-flag)
		:energy         energy-par-plant
		:strategy-storage strategy-storage
		:strategy-executed first-execut)))

(defun init-plans-array ()
  (array-map
   #'(lambda (n) n (init-plant))
   (make-array (list *field-height* *field-width*))))


;; update-plants-list ;;;;;;;;;;
;; strategy, storage

(defun get-plant-strategy (plant)
  (mapcar
   #'(lambda (storage)
       (funcall (plant-strategy-func plant) storage))
   (plant-strategy-storage plant)))

(defun plant-coefficient-matrix (execut-array)
  (let* ((height (1- (car  (array-dimensions execut-array))))
	 (width  (1- (cadr (array-dimensions execut-array))))
	 (coord-mat (coordinate-matrix width height)))
    (array-map
     #'(lambda (coord)
	 (let*
	     ((y (car coord))
	      (x (cadr coord))
	      (above-str (nth *under* (aref execut-array (if (= y 0) height (1- y)) x)))
	      (under-str (nth *above* (aref execut-array (if (= y height) 0 (1+ y)) x)))
	      (right-str (nth *left*  (aref execut-array y (if (= x width) 0 (1+ x )))))
	      (left-str  (nth *right* (aref execut-array y (if (= x 0) width (1- x) )))))
	   (list above-str right-str under-str left-str)))
     coord-mat)))

;; energy
(defun energy-coefficient-list (own-str partner-str)
  (mapcar #'(lambda (own part)
	      (cond ((and (eq own *cooperation*) (eq part *cooperation*)) 4)
		    ((and (eq own *cooperation*) (eq part *betrayal*))    0)
		    ((and (eq own *betrayal*)    (eq part *cooperation*)) 5)
		    ((and (eq own *betrayal*)    (eq part *betrayal*))    1)))
	  own-str partner-str))

(defun distributed-energy (coefficient-array-list energy-array
			   latch-energy width height)
  (let* ((field-all-coeffigent
	  (reduce #'+ (flatten (2d-array-to-list coefficient-array-list))))
	 (energy-par-coefficient (/ (* latch-energy width height) field-all-coeffigent))
	 (energy-array
	  (array-map #'(lambda (ene coef-list)
			 (float (+ (- latch-energy) ene
				   (* (reduce #'+ coef-list) energy-par-coefficient))))
		     energy-array coefficient-array-list)))
    energy-array))

;; genaration-exchange
(defun is-dead (plant)
  (>= 0 (plant-energy plant)))

(defun generation-exchange (plant-array width height)
  (array-map
   #'(lambda (p coord)
       (let* ((x (cadr coord))
	      (y (car coord))
	      (height_d (1- height))
	      (width_d (1- width))
	      (above-plant (aref plant-array (if (= y 0) height_d (1- y)) x))
	      (under-plant (aref plant-array (if (= y height_d) 0 (1+ y)) x))
	      (right-plant (aref plant-array y (if (= x width_d) 0 (1+ x ))))
	      (left-plant  (aref plant-array y (if (= x 0) width_d(1- x) )))
	      (adj-plant (list above-plant right-plant under-plant left-plant))
	      (highest-energy-plant (car  (sort (copy-list adj-plant) #'>
						:key #'plant-energy))))
	 (if (is-dead p)
	     (make-plant
	      :strategy-func (plant-strategy-func highest-energy-plant)
	      :strategy-flag (plant-strategy-flag highest-energy-plant)
	      :strategy-storage *first-plant-storage*
	      :strategy-executed *first-plant-execut*
	      :energy (plant-energy p))
	     (make-plant
	      :strategy-func (plant-strategy-func p)
	      :strategy-flag (plant-strategy-flag p)
	      :strategy-storage (mapcar
				 #'(lambda (plant storage)
				     (if (is-dead plant) (car *first-plant-storage*)
					 storage))
				 adj-plant (plant-strategy-storage p))
	      :strategy-executed (mapcar
				  #'(lambda (plant strategy)
				      (if (is-dead plant) *cooperation*
					  strategy))
				  adj-plant (plant-strategy-executed p))
	      :energy (plant-energy p)))))
   plant-array (coordinate-matrix (1- width) (1- height))))


(defun update-plants-array (field)
  (let*
      ((latch-energy (field-latch-energy field))
       (plants-array (field-plants-array field))
       
       ;; strategy, storage
       (execut-array (array-map #'get-plant-strategy
				plants-array))
       (executed-array (plant-coefficient-matrix execut-array))
       (coefficient-array-list (array-map #'energy-coefficient-list
					  execut-array executed-array))
       ;; energy
       (old-energy-array (array-map #'plant-energy plants-array))
       (energy-array (distributed-energy coefficient-array-list old-energy-array 
					 (field-latch-energy field)
					 (field-width field) (field-height field)))
       ;; generation-excahnge
       (acted-plants
	(array-map
	 #'(lambda (p executed ene)
	     (make-plant
	      :strategy-func (plant-strategy-func p)
	      :strategy-flag (plant-strategy-flag p)
	      :strategy-executed executed 
	      :energy ene
	      :strategy-storage (mapcar #'roll-list executed (plant-strategy-storage p))))
	 plants-array executed-array energy-array))
       
       (generation-exchanged-plants
	(generation-exchange acted-plants (field-width field) (field-height field))))
    generation-exchanged-plants))




;; print-plants-list ;;;;;;;;;;
(defun print-plants-list (plants-array)
  (show-board
   (array-map
    #'(lambda (x)
	(case (plant-strategy-flag x)
	  (all-a 'a) (all-d 'd) (tft 't) (otherwise 'n)))
    plants-array)))


;; simulator ;;;;;;;;;;;;;;;;;

(defun init-field ()
  (make-field
   :plants-array (init-plans-array)
   :width *field-width*
   :height *field-height*
   :exist-energy *field-energy*
   :latch-energy *field-latch-energy*
   :each-plants-storage-size *field-each-storage*))


(defun read-command ())

(defun update-field (field)
  (make-field
   :width (field-width field)
   :height (field-height field)
   :passed-turn (1+ (field-passed-turn field))
   :plants-array (update-plants-array field)
   :exist-energy (field-exist-energy field)
   :latch-energy (field-latch-energy field)
   :each-plants-storage-size (field-each-plants-storage-size field)))


(defun print-field (field)
  (format t "turn ~d~%" (field-passed-turn field))
  (print (aref (field-plants-array field) 1 1)) (format t "~%")
  (print-plants-list (field-plants-array field)))


(defun simulate-loop (field)
  (let* ((new-field (update-field field)))
    (print-field new-field)
    (if (string-equal (read-line) "exit")
	nil
	(simulate-loop new-field))))

(defun start-simulate ()
  (simulate-loop (init-field )))

