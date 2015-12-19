
;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *window-width* 640)
(defparameter *window-height* 480)


(defun draw-field (field)
  (let* ((field-width (field-width field))
	 (field-height (field-height field)))
    (array-map
     #'(lambda (p coord)
	 (draw-filled-circle-*
	  (+ 10 (* 15 (cadr coord)))
	  (+ 10 (* 15 (car coord)))
	  7
	  :color (case (plant-strategy-flag p)
		   (tft *green*)
		   (all-a *blue*)
		   (all-d *red*))))
     (field-plants-array field) (coordinate-matrix (1- field-width) (1- field-height)))))


(defun UI ()
  (with-init ()
    (window *window-width* *window-height* :title-caption "simulator")
    (setf (frame-rate) 2)
    (update-display)
    (initialise-default-font *font-10x20*)
    
    (let* ((field (init-field)))
      (with-events ()
	(:quit-event () 't)
	(:idle
	 nil
	 (clear-display *black*)
	 
	 (setq field (update-field field))
	 (draw-field field)
	 (draw-string-solid-* (format nil "~,2F" (field-passed-turn field))
	  400 400)
	 (print (show-board (array-map #'(lambda (p) (round(plant-energy p)))
				       (field-plants-array field))))
	 
	 (update-display))
	
	))))


