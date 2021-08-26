#!/bin/sh

PORT=${PORT:-5000}
CLACK_HANDLER=${CLACK_HANDLER:-hunchentoot}

exec .qlot/bin/clackup -s github-webhook/server \
  --address 0.0.0.0 \
  --port 5000 \
  --server "$CLACK_HANDLER" \
  --debug nil \
  /app/app.lisp
