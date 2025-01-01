FROM debian:bookworm-slim

# 1) Update apt and install dependencies (curl & sbcl)
RUN apt-get update && \
    apt-get install -y sbcl curl && \
    rm -rf /var/lib/apt/lists/*

# 2) Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
         --quit && \
    rm quicklisp.lisp

# 3) Make Quicklisp available for future runs
RUN echo '(load "~/.quicklisp/setup.lisp")' >> ~/.sbclrc

# 4) Copy your Lisp app
WORKDIR /app
COPY app.lisp /app/

# 5) Expose the port for Hunchentoot
EXPOSE 8081

# 6) Run your application. Make sure `app.lisp` calls (wait-for-acceptor *server*) in start-todo-app
CMD ["sbcl", "--load", "app.lisp", "--eval", "(todo-app:start-todo-app)"]
