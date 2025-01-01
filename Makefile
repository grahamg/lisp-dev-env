.PHONY: build up down shell run emacs

# Build all images (both dev_env and hunchentoot_app)
build:
	docker-compose build

# Spin up the dev environment in the background
up:
	docker-compose up -d dev_env

# Stop and remove containers/networks
down:
	docker-compose down

# Enter a shell inside the dev environment container
shell:
	docker exec -it dev_env_lisp /bin/bash

# Build and run the Hunchentoot app (foreground)
run:
	docker-compose up --build hunchentoot_app

# Attach directly into Doom Emacs (terminal mode) in dev_env
emacs:
	docker exec -it dev_env_lisp bash -c "doom emacs"
