(ql:quickload '(cl-who hunchentoot parenscript))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package retro-games)

(defclass game ()
  ((name  :reader   name
          :initarg  :name)
   (votes :accessor votes
          :initform 0))
  (:documentation "Name instances via make-instance. Vote via accessor"))

(defmethod vote-for (user-selected-game) (incf (votes user-selected-game)))

(defvar *games* '())

(defun game-from-name (name)
  (find name *games* :test #'string-equal
                     :key #'name))

(defun game-stored? (game-name) (game-from-name game-name))

(defun games ()
  "Return a list of games in *games* sorted by votes"
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  "Create a game instance using make-instance and push it onto *games*"
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))

(defmethod print-object ((object game) stream)
  "Specializing generic function print-object for game class
   This allows e.g. game-from-name to show us slot values."
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(setf (html-mode) :html5) ; output in HTML5 from now on

;; Demo CL-WHO function with-html-output
(with-html-output (*standard-output* nil :prologue t :indent t)
  (:html
   (:head
    (:title "Test page"))
   (:body
    (:p "CL-WHO is really easy to use"))))

(defmacro standard-page ((&key title) &body body)
  "Creates a standard page using code supplied in params"
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css"))
            (:body
             (:div :id "header"
                   (:img :src "/logo.jpg"
                         :alt "Commodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favourite retro game"))
             ,@body))))

;; usage example
(standard-page
    (:title "Retro Games")
  (:h1 "Top Retro Games")
  (:p "We'll write the code later..."))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(start-server 8080)

;; push a route onto the acceptor's (or servers? )*dispatch-table*
(push (create-prefix-dispatcher "/retro-games.htm"
                                'retro-games)
      *dispatch-table*)

(defun retro-games ()
  (standard-page (:title "Retro Games")
    (:h1 "Top Retro Games")
    (:p "We'll write the code later")))
