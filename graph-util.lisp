(ql:quickload :uiop)
(defpackage #:graph-util
  (:use #:cl)
  (:import-from #:uiop
		#:run-program)
  (:export #:png<-graph
	   #:png<-ugraph))
(in-package #:graph-util)
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))
(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))
(defparameter *label-beg* "[label=\"")
(defparameter *label-end* "\"];")
(defun dot<-nodes (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (mapc #'princ (list
			 (dot-name (car node))
			 *label-beg*
			 (dot-label node)
			 *label-end*)))
	nodes))
(defun dot<-edges (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (mapc #'princ (list (dot-name (car node))
				      "->"
				      (dot-name (car edge))
				      *label-beg*
				      (dot-label (cdr edge))
				      *label-end*)))
		(cdr node)))
	edges))
(defun dot<-graph (nodes edges)
  (princ "digraph{")
  (dot<-nodes nodes)
  (dot<-edges edges)
  (princ "}"))
(defun png<-dot (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (uiop:run-program (concatenate 'string "dot -Tpng -O " fname)))
(defun png<-graph (fname nodes edges)
  "Visualize a directed graph"
  (png<-dot fname (lambda ()
		    (dot<-graph nodes edges))))
(defun dot<-uedges (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (mapc #'princ (list (dot-name (caar lst))
					   "--"
					   (dot-name (car edge))
					   *label-beg*
					   (dot-label (cddr edge))
					   *label-end*))))
		   (cdar lst)))
	   edges))
(defun dot<-ugraph (nodes edges)
  (princ "graph{")
  (dot<-nodes nodes)
  (dot<-uedges edges)
  (princ "}"))
(defun png<-ugraph (fname nodes edges)
  "Visualize an undirected graph"
  (png<-dot fname (lambda ()
		    (dot<-ugraph nodes edges))))
