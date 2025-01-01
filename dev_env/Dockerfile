FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# 1) Basic environment setup
RUN apt-get update && apt-get install -y --no-install-recommends \
    sbcl \
    curl \
    wget \
    tree \
    git \
    build-essential \
    nano \
    tig \
    netcat \
    net-tools \
    emacs \
 && rm -rf /var/lib/apt/lists/*

# 2) Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --eval '(quit)' \
 && rm quicklisp.lisp

# 3) Install SLIME (and its dependencies) via Quicklisp
RUN sbcl --non-interactive \
    --eval '(ql:quickload :quicklisp-slime-helper)' \
    --eval '(quit)'

# 4) Install Doom Emacs
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d \
    && ~/.emacs.d/bin/doom install --force

# 5) Create a non-root user for development
RUN useradd -ms /bin/bash devuser

# Switch to devuser
USER devuser
WORKDIR /home/devuser

# 6) Optional symlink for Doom command
RUN ln -s /root/.emacs.d/bin/doom /usr/local/bin/doom || true

# 7) Minimal Doom Emacs config for SLIME
RUN mkdir -p /home/devuser/.doom.d && \
    echo '(use-package! slime
  :init
  (setq inferior-lisp-program "sbcl"))' \
    > /home/devuser/.doom.d/config.el && \
    echo '(doom! :input
       :completion
       :ui
       :editor
       :emacs
       :checkers
       :tools
       :lang
       :config)' \
    > /home/devuser/.doom.d/init.el && \
    /root/.emacs.d/bin/doom sync --force && \
    chown -R devuser:devuser /home/devuser/.doom.d

CMD ["/bin/bash"]
