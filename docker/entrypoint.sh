#!/bin/sh

exec .qlot/bin/clackup -s docker-gh-webhook app.lisp \
  --address 0.0.0.0 --port 5000 --debug nil
