(defparameter *width* 100)
(defparameter *height* 30)
;; coords: (left top width height)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
     (let ((pos (cons (+ left (random width)) (+ top (random height)))))
       (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))
