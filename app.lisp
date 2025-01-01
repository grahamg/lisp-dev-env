;;;; 1) Load Hunchentoot
(ql:quickload :hunchentoot)

;;;; 2) Define our package and export our start/stop symbols
(defpackage :todo-app
  (:use :cl :hunchentoot)
  (:export :start-todo-app :stop-todo-app))

(in-package :todo-app)

;;;; 3) A simple list to hold our tasks
(defparameter *todo-list* '())

;;;; 4) Renders a simple HTML page
(defun render-todo-list ()
  (format nil
          "<html>
             <body>
               <h1>TODO List</h1>
               <ul>
                 ~{<li>~a</li>~}
               </ul>
               <form method='POST' action='/add'>
                 <input type='text' name='task' placeholder='New Task' required />
                 <button type='submit'>Add Task</button>
               </form>
             </body>
           </html>"
          *todo-list*))

;;;; 5) Display the main page (GET /)
(define-easy-handler (todo-list :uri "/") ()
  (render-todo-list))

;;;; 6) Add-task handler (POST /add)
(define-easy-handler (add-task :uri "/add") (task)
  "Handle POST requests with a 'task' parameter."
  ;; `*request*` is provided by DEFINE-EASY-HANDLER at runtime.
  (when (and (string= (request-method *request*) "POST")
             task)
    (push task *todo-list*))
  (redirect "/"))

;;;; 7) Create and manage the server
(defvar *server* (make-instance 'easy-acceptor :port 8081))

(defun start-todo-app ()
  (start *server*)
  ;; Simple blocking approach (one day at a time in an endless loop)
  (loop (sleep 86400)))

(defun stop-todo-app ()
  (stop *server*))

