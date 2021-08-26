FROM fukamachi/qlot AS build-env

WORKDIR /app
COPY . /app
RUN qlot install

FROM fukamachi/sbcl
COPY --from=build-env /app/.qlot /app/.qlot

ENV GH_HOOKS_DIR /app/hooks
ENV CLACK_HANDLER hunchentoot
ENV QUICKLISP_HOME /app/.qlot/

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends openssl && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

RUN set -x; \
  ros -s clack -s github-webhook/server && \
  mkdir -p /app/hooks

ENTRYPOINT ["/app/docker/entrypoint.sh"]
