;; (error "foo")

(define-condition foo () ()
  (:report (lambda (condition stream)
	     (break)
	     (princ "Stop itttt!!" stream))))

(defun bad-function ()
  (error 'foo))

;; (handler-case (bad-function)
;;   (foo () "foo happened")
;;   (bar () "bar happened"))
