(ql:quickload '("hunchentoot" "caveman" "spinneret" "djula"))

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))

(setf (hunchentoot:acceptor-document-root *acceptor*) #p"~/code/common-lisp/cl-web/")

(defvar *my-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4444
                                                                :document-root #p"www/"))

(defun hello ()
  (format nil "Hello, it works"))

(push
 (hunchentoot:create-prefix-dispatcher "/hello.html" #'hello)
 hunchentoot:*dispatch-table*)

; better way using hunchentoot macro
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name)) ;conditional format if name exists
