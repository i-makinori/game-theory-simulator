(in-package #:game-theory-simulator)

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

;; table of strategy point
(defun energy-coefficient-list (own-str partner-str)
  (mapcar #'(lambda (own part)
              (cond ((and (eq own *cooperation*) (eq part *cooperation*)) 0)
                    ((and (eq own *cooperation*) (eq part *betrayal*))    0)
                    ((and (eq own *betrayal*)    (eq part *cooperation*)) 10)
                    ((and (eq own *betrayal*)    (eq part *betrayal*))    10)))
          own-str partner-str))
