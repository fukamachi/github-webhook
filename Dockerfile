FROM fukamachi/qlot AS build-env

WORKDIR /app
COPY . /app
RUN qlot install

FROM fukamachi/sbcl
COPY --from=build-env /app/.qlot /app/.qlot

ENV GH_HOOKS_DIR /app/hooks
ENV CLACK_HANDLER hunchentoot

WORKDIR /app
COPY . /app

RUN set -x; \
  QUICKLISP_HOME=.qlot/ ros -s clack -s github-webhook/server && \
  mkdir -p /app/hooks

ENTRYPOINT ["/app/docker/entrypoint.sh"]
