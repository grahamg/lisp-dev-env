FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl \
    curl \
    wget \
    tig \
    tree \
    git \
    build-essential \
 && rm -rf /var/lib/apt/lists/*

# 1) Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --eval '(quit)' \
 && rm quicklisp.lisp

# 2) Pre-install Hunchentoot (and dependencies)
RUN sbcl --non-interactive \
    --eval '(ql:quickload :hunchentoot)' \
    --eval '(quit)'

# 3) Copy your application code
WORKDIR /usr/src/app
COPY app.lisp .

# 4) Expose port 8084
EXPOSE 8084

# 5) Run Hunchentoot on container start
#    This loads your app code and starts the server on port 8080.
CMD ["sbcl", \
     "--eval", "(ql:quickload :hunchentoot)", \
     "--eval", "(load \"app.lisp\")", \
     "--eval", "(my-hunchentoot-app:start-server)", \
     "--eval", "(sb-thread:join-thread sb-ext:*main-thread*)"]
