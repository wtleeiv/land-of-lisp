;; pg. 123 (ch. 6.5)

;; symbols get upcased, so don't worry about capitals
(defparameter *nodes* '((living-room
			 (you are in the living room.
			  a wizard is snoring loudly on the couch.))
			(garden
			 (you are in a beutiful garden.
			  there is a well in front of you.))
			(attic
			 (you are in the attic.
			  there is a giant welding torch in front of you.))))

;; functional style :: pass in nodes, don't hardcode ref internally
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room
			 (garden west door)
			 (attic upstairs ladder))
			(garden
			 (living-room east door))
			(attic
			 (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey . living-room)
				   (bucket . living-room)
				   (frog . garden)
				   (chain . attic)))

;; REMOVE-if-not maps over list, passed each elem to fn
;; in this case, at-loc-p
(defun objects-at (loc objs obj-locs)
  (flet ((at-loc-p (obj)
	   (eq loc (cdr (assoc obj obj-locs)))))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (flet ((obj-msg (obj)
	   `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'obj-msg (objects-at loc objs obj-locs)))))

;; track player position
;; start out in the living room
(defparameter *location* 'living-room)

;; impure fns...easier for user
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; macro so user doesn't have to quote direction
(defmacro walk (direction)
  `(let ((new-loc-edge (find ',direction
			     (cdr (assoc *location* *edges*))
			     :key #'cadr)))
     (if new-loc-edge
	 (progn (setf *location* (car new-loc-edge))
		(look))
	 '(you cannot go that way.))))

;; finding objects uses assoc
;; assoc returns *first* entry that matches
;; so it's fine to push new objects onto location list
(defmacro pickup (object)
  `(if (member ',object (objects-at *location* *objects* *object-locations*))
       (progn (push '(,object . body) *object-locations*)
	      '(you are now carrying the ,object))
       '(you cannot pick that up.)))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string
					    "("
					    (read-line)
					    ")"))))
    #| quote args if using non-macro user functions
    (flet ((quote-it (x)
    (list 'quote x)))
    (cons (car cmd) (mapcar #'quote-it (cdr cmd))))
    |#
    cmd))

(defvar *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do now know that command.)))

(defun tweak-text (lst caps lit)
  (when lst ; retuwns nil when lst empty, start consing up list onto nil
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space)
	     (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.))
	     (cons item (tweak-text rest t lit)))
	    ((eq item #\")
	     (tweak-text rest caps (not lit)))
	    (lit
	     (cons item (tweak-text rest nil lit)))
	    (caps
	     (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t
	     (cons (char-downcase item) (tweak-text rest caps lit)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
