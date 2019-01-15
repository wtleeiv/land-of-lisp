(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 10)

;;; Helper Function

(defun randval (n)
  (1+ (random (max n 1))))

;;; Monsters

(defstruct monster
  (health (randval 10)))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

(defmethod monster-hit (m d)
  (decf (monster-health m) d)
  (if (monster-dead m)
      (progn (princ "You killed the ")
	     (princ (type-of m))
	     (princ "! Fuck that bitch! "))
      (progn (princ "You railed the ")
	     (princ (type-of m))
	     (princ " ")
	     (princ d)
	     (princ " times! "))))

;; Orc

(defstruct (orc (:include monster))
  (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((d (randval (orc-club-level m))))
    (decf *player-health* d)
    (princ "An orc beats the shit out of you with its club ")
    (princ d)
    (princ " times! ")))

;; Hydra

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads"))

(defmethod monster-attack ((m hydra))
  (let ((d (randval (ash (monster-health m) -1))))
    (decf *player-health* d)
    (incf (monster-health m))
    (princ "A hydra attacks you with ")
    (princ d)
    (princ " of its heads! It also grows yet another head! ")))

(defmethod monster-hit ((m hydra) d)
  (decf (monster-health m) d)
  (if (monster-dead m)
      (princ "The mangled corpse of the fully decapitated hydra falls lifelessly to the ground!")
      (progn (princ "You cut off ")
	     (princ d)
	     (princ " of the hydra's heads! "))))

;; Slime Mold

(defstruct (slime-mold (:include monster))
  (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((d (randval (slime-mold-sliminess m))))
    (princ "A slime mold jizzes on your legs, decreasing your agility by ")
    (princ d)
    (princ "! ")
    (decf *player-agility* d)
    (when (zerop (random 2))
      (princ "It also cums on your face! ")
      (decf *player-health*))))

;; Brigand

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((d (max *player-health* *player-agility* *player-strength*)))
    (cond ((= d *player-health*)
	   (princ "A brigand nails you in both testicles with its slingshot! ")
	   (decf *player-health* 2))
	  ((= d *player-agility*)
	   (princ "A brigand whips you like the nubile slave you are! You limp away in submission! ")
	   (decf *player-agility* 2))
	  ((= d *player-strength*)
	   (princ "A brigand teases you with a feather! You feel weak with desire! ")
	   (decf *player-strength* 2)))))

;;; Monster Helpers

(defun init-monsters ()
  (setf *monsters* (map 'vector
			(lambda (x)
			  (funcall (nth (random (length *monster-builders*))
					*monster-builders*)))
			(make-array *monster-num*))))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((i 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "    ")
	   (princ (incf i))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defun random-monster ()
  (let ((m (aref *monsters* (random *monster-num*))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #: ")
  (let (( x (read)))
    (if (and (integerp x) (>= x 1) (<= x *monster-num*))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster)))))

;;; Player

(defun init-player ()
  (setf *player-health* 30
	*player-agility* 30
	*player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

;;; Game

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead) ;; unnecessary?
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (fresh-line)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (fresh-line)
    (princ "Congratulations! You have vanquished all of your foes.")))
