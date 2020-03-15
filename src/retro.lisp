(ql:quickload '(cl-who hunchentoot parenscript cl-mongo))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package retro-games)

(defclass game ()
  ((name  :reader   name
          :initarg  :name)
   (votes :accessor votes
          :initarg votes ; when read from persistent storage
          :initform 0)))

(defun doc->game (game-doc)
  (make-instance 'game :name (get-element "name" game-doc)
                       :votes (get-element "votes" game-doc)))

(defmethod vote-for (game) (incf (votes game)))

(defmethod vote-for :after (user-selected-game) ; rewrite as :after method
  ;; this allows us to separate persistence that fires when someone votes
  (let ((game-doc (game->doc user-selected-game)))
    (db.update *game-collection* ($ "name" (name user-selected-game))game-doc)))

(defvar *games* '())

;; now we add persistence

;; use mongo - https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/

(cl-mongo:db.use "games") ; initialise a mongodb

(defparameter *game-collection* "game") ; added to support persistence

(defun game-from-name (name)
  "Look for named game in db and if found, restore to CLOS"
  (let ((found-games (docs (db.find *game-collection*
                                    ($ "name" name)))))
    (when found-games
      (doc->game (first found-games)))))

(defun game-stored? (name)
  (game-from-name name))

(defun games ()
  "Return a list of games in games db,  sorted by votes"
  (mapcar #'doc->game
          (docs (iter
                 (db.sort *game-collection*
                          :all
                          :field "votes"
                          :asc nil)))))

(defun game->doc (game)
  ($ ($ "name" (name game))
     ($ "votes" (votes game))))

(defun add-game (name)
  "Create a game instance using make-instance and push it onto *games*
   Modified to allow for storing games in mongo"
  (let ((game (make-instance 'game :name name)))
    (db.insert *game-collection* (game->doc game))))

(defun unique-index-on (field)
  (db.ensure-index *game-collection*
                   ($ field 1)
                   :unique t))

(unique-index-on "name") ; execute this to ensure unique index

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

(defmacro standard-page ((&key title script) &body body)
  "Creates a standard page using code supplied in params.
   We can build this up into a DSL for doing our website"
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css") ; we add the script tags now
             ,(when script ; if script is present, adds script
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "header"
                   (:img :src "~/code/common-lisp/cl-web/logo.jpg"
                         :alt "Commodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favourite retro game"))
             ,@body))))

;; usage example
; (standard-page
;    (:title "Retro Games")
;  (:h1 "Top Retro Games")
;  (:p "We'll write the code later..."))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(start-server 8080)

;; push a route onto the acceptor (or server? ) *dispatch-table*

;; (push (create-prefix-dispatcher "/retro-games.htm"
;;                                'retro-games)
;;      *dispatch-table*)

;; (defun retro-games ()
;;  "Which points to this handler"
;;  (standard-page (:title "Retro Games")
;;    (:h1 "Top Retro Games")
;;    (:p "We'll write the code later")))

;; Hunchentoot has a macro (in effect a DSL) to make the above steps easier, so.
(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page ; Our DSL for web pages based on CL-WHO
      (:title "Top Retro Games")
    (:h1 "Vote for your all-time favourite retro games")
    (:p "Missing a game? Make it available to vote on"
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games)) ;terate over game in games
             (htm
              (:li (:a :href (format nil "vote?name=~a"
                                     (url-encode
                                      (name game))) "vote!")
                   (fmt "~A with ~d votes" (escape-string (name game))
                        (votes game)))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                  :script (ps ; add client side validation via script param
                            (defvar add-form nil)
                            (defun validate-game-name (evt)
                              (when (= (@ add-form name value "")
                                       (chain evt (prevent-default))
                                       (alert "Please enter a name"))))
                            (defun init ()
                              (setf add-form (chain document
                                                    (get-element-by-id "addform")))
                              (chain add-form
                                     (add-event-listener "submit"
                                                         validate-game-name false)))
                            (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "what is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

;;; now lets try to make the client side work a bit better - using Parenscript

;; first, we need a :script bit in our standard-page macro to hold our js
