;; A minimal Hunchentoot server demonstration

(defpackage :my-hunchentoot-app
  (:use :cl :hunchentoot))

(in-package :my-hunchentoot-app)

(defparameter *my-port* 8080
  "Port on which Hunchentoot will listen.")

(defclass hello-dispatcher (dispatch-handler) ())

(defmethod handle-request ((handler hello-dispatcher) (request request))
  (declare (ignore handler))
  (setf (content-type* request) "text/plain")
  (with-output-to-string (out)
    (format out "Hello from Hunchentoot on port ~A!~%" *my-port*)))

(defun start-server ()
  (setf *dispatch-table*
        (list (make-instance 'hello-dispatcher :regexp ".*")))
  (start (make-instance 'easy-acceptor :port *my-port*))
  (format t "Server running at http://localhost:~A/~%" *my-port*))

(defun stop-server ()
  (stop (find-acceptor-by-port *my-port*))
  (format t "Server on port ~A stopped.~%" *my-port*))
