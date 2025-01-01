Lisp WebApp Starter (Hunchentoot)

A minimal Common Lisp (CL) web application using Hunchentoot. This repository includes Dockerfiles to run the app in a container with Quicklisp installed.

Features
	•	Hunchentoot web server: Serves HTTP on port 8081.
	•	Todo List Example: Add, view, and manage tasks in memory.
	•	Docker Integration: Quickly spin up the app using Docker or Docker Compose.
	•	Quicklisp Setup: Automatically installed into the image for easy library management.

Requirements
	•	Docker (if you’re using the Docker approach)
	•	SBCL and Quicklisp (if running locally without Docker)

Getting Started

1. Clone the Repo

git clone https://github.com/grahamg/lisp-webapp-starter.git
cd lisp-webapp-starter

2. Build & Run with Docker Compose

docker-compose up --build

	•	The server should be accessible at http://localhost:8080.
	•	By default, Docker exposes port 8080 to the host. If you need a different port, edit the docker-compose.yml.

3. Local Development (Optional)

If you prefer running locally:
	1.	Ensure SBCL + Quicklisp are installed.
	2.	Load app.lisp in SBCL:

sbcl --load app.lisp --eval '(todo-app:start-todo-app)'


	3.	Navigate to http://localhost:8081.

	Note: If you run locally, be sure you have loaded (ql:quickload :hunchentoot) or included it in your setup.

Project Structure

.
├── app.lisp             ; The main Lisp application code
├── docker-compose.yml   ; Docker Compose config
├── Dockerfile           ; Docker build instructions
├── .gitignore           ; Common Lisp & Docker ignore rules
├── README.md            ; This file
└── ...

	•	app.lisp: Defines the routes (handlers) and server startup.
	•	Dockerfile: Builds the container using Debian or another base with SBCL + Quicklisp.
	•	docker-compose.yml: Simplifies multi-container orchestration or easy building/running.
	•	.gitignore: Excludes compiled Lisp files, Quicklisp, editor backups, etc.

Usage
	1.	Add Task: Type a new task in the text box, then click “Add Task.”
	2.	View Tasks: All current tasks display as a list of <li> items.
	3.	Data Persistence: Currently, tasks are in-memory only. If you restart the server, you lose them. Adapt the code to store tasks in a database or file as needed.

Customizing
	•	Routes: Add new routes using (define-easy-handler (my-handler :uri "/my-uri") (params) ...).
	•	Templates: Render dynamic content by adjusting render-todo-list or creating more functions for different pages.
	•	Port: In app.lisp, edit :port 8080 to change the server port. Also update Docker’s EXPOSE 8080 if needed.

Troubleshooting
	•	SBCL Exits Immediately: Ensure start-todo-app includes a blocking loop ((loop (sleep 86400))) so the container or process stays alive.
	•	Dependency Issues: Confirm docker-compose build completes successfully and installs Quicklisp + SBCL without errors.
	•	Platform Mismatch (ARM vs. AMD64): If you’re on an M1/M2 Mac (ARM), use an ARM-compatible base image or build SBCL from source.

Contributing
	1.	Fork the repo
	2.	Create a feature branch (git checkout -b my-feature)
	3.	Commit your changes (git commit -am 'Add my feature')
	4.	Push to the branch (git push origin my-feature)
	5.	Open a Pull Request

License

This project is distributed under the MIT License. See LICENSE for details (replace with the appropriate license if different).

Enjoy hacking on Common Lisp with Hunchentoot! If you have any questions or run into issues, feel free to open an issue or submit a pull request.
