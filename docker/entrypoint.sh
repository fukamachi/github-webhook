#!/bin/sh

PORT=${PORT:-5000}
CLACK_HANDLER=${CLACK_HANDLER:-hunchentoot}

exec sbcl --noinform --non-interactive --load .qlot/setup.lisp \
  --eval '(ql:quickload (list :clack :github-webhook/server))' \
  --eval "(clack:clackup #P\"app.lisp\" :address \"0.0.0.0\" :port 5000 :server :$CLACK_HANDLER :use-thread nil :debug nil)"
