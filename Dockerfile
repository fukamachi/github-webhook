FROM fukamachi/qlot AS build-env

WORKDIR /app
COPY . /app
RUN qlot install

FROM clfoundation/sbcl
ENV GH_HOOKS_DIR /app/hooks
ENV CLACK_HANDLER hunchentoot

WORKDIR /app
RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"
COPY --from=build-env /app/.qlot /app/.qlot
COPY . /app
RUN set -x; \
  sbcl --load .qlot/setup.lisp --eval '(ql:quickload (list :clack :github-webhook/server))' && \
  mkdir -p /app/hooks

ENTRYPOINT ["/app/docker/entrypoint.sh"]
