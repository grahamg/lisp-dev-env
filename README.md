# lisp-dev-env

This repository demonstrates a multi-container setup for building and running a **Hunchentoot** web application in Common Lisp (SBCL). It also includes:

- **dev_env**: An Ubuntu-based container with SBCL, Quicklisp, **Doom Emacs**, and SLIME for development.
- **hunchentoot_app**: A container that runs the Hunchentoot app on port **8080**.
- A **Makefile** for convenient commands (building, running, attaching Emacs, etc.).
- A **docker-compose.yml** for orchestrating both containers, and optionally mounting your `.gitconfig` for a smoother dev workflow.

---

## Directory Structure

```
lisp-dev-env/
├── docker-compose.yml
├── dev_env/
│   └── Dockerfile
├── hunchentoot_app/
│   ├── app.lisp
│   └── Dockerfile
├── Makefile
├── .gitignore
└── README.md
```

### Files

1. **docker-compose.yml**  
   - Defines the `dev_env` (for development) and `hunchentoot_app` (for running the server).
   - Mounts your local `.gitconfig` and the project folder into the dev container.

2. **dev_env/Dockerfile**  
   - Installs SBCL, Quicklisp, SLIME, **Doom Emacs**, plus various build tools.
   - Creates a non-root user (`devuser`) for better security.
   - Sets up a minimal Doom Emacs config that loads SLIME with SBCL.

3. **hunchentoot_app/Dockerfile**  
   - Installs SBCL and Quicklisp.
   - Preloads Hunchentoot.
   - Copies `app.lisp` and starts the server on port **8084**.

4. **hunchentoot_app/app.lisp**  
   - Minimal Hunchentoot server that responds with “Hello from Hunchentoot…” on any route.

5. **Makefile**  
   - Common targets: `build`, `up`, `down`, `shell`, `run`, and `emacs`.
   - `make emacs` attaches to Doom Emacs running inside the dev_env container in terminal mode.

6. **.gitignore**  
   - Ignores common Lisp artifacts (`*.fasl`, etc.), Quicklisp caches, Emacs backups, Docker ephemeral files.

---

## Quick Start

1. **Clone** or create this repo and `cd` into it:
   ```bash
   git clone https://github.com/grahamg/lisp-dev-env <project-name>
   cd <project-name>
   ```

2. **(Optional) Export your UID/GID** so the dev container runs as your local user:
   ```bash
   export UID=$(id -u)
   export GID=$(id -g)
   ```
   This step helps avoid file-permission issues.

3. **Build everything**:
   ```bash
   make build
   ```
   This runs `docker-compose build` for both the dev environment and the Hunchentoot app.

4. **Spin up** the dev environment:
   ```bash
   make up
   ```
   This starts `dev_env` in the background. It also volume-mounts your `.gitconfig` and current directory if specified in `docker-compose.yml`.

5. **Attach** to the dev environment shell:
   ```bash
   make shell
   ```
   You’re now inside the container as `devuser`. You can run commands like:
   ```bash
   sbcl
   doom emacs
   git ...
   ```

6. **Open Doom Emacs** directly:
   ```bash
   make emacs
   ```
   This attaches a **terminal-based** Doom Emacs session (with SLIME configured).  
   - Once inside Emacs, do `M-x slime` to start a Lisp REPL, or open a `.lisp` file and hack away.

7. **Run** the Hunchentoot app:
   ```bash
   make run
   ```
   This builds and runs the `hunchentoot_app` container, exposing port **8080**.  
   - Open [http://localhost:8084](http://localhost:8084) to see “Hello from Hunchentoot on port 8080!”

8. **Stop** everything:
   ```bash
   make down
   ```
   This stops and removes the containers and Docker network.

---

## Notes & TODOs

- **SLIME + Doom Emacs**: Already configured by default, pointing to SBCL. For using SLY or custom settings, update `~/.doom.d/config.el` inside the container.
- **Testing**: For production usage, add tests, integrate CI, or explore staging vs. production configs.
- **Secrets & Security**: Use Docker secrets or environment variables.
- **Hunchentoot**: The bundled web application is merely a placeholder. Implement more routes, middlewares, or static file serving as needed.

