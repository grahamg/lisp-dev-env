#!/usr/bin/env bash
set -e
sbcl --load app.lisp --eval '(todo-app:start-todo-app)'
# now keep the container running indefinitely
tail -f /dev/null
