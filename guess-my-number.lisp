(defvar *small*)
(defvar *big*)
(defvar *guess*)

;; ash :: arithmetic shift
;; here it divides by two
(defun guess-my-number (&optional (small 1) (big 100))
  (setf *small* small
	*big* big
	*guess* (ash (+ *big* *small*) -1)))

(defun smaller ()
  (setf *big* (1- *guess*))
  (guess-my-number *small* *big*))

(defun bigger ()
  (setf *small* (1+ *guess*))
  (guess-my-number *small* *big*))
