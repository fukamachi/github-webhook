#!/bin/sh

exec .qlot/bin/clackup -s github-webhook/server app.lisp \
  --address 0.0.0.0 --port 5000 --debug nil
