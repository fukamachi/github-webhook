ARG SBCL_VERSION=2.0.9
FROM fukamachi/sbcl:${SBCL_VERSION}
ARG QLOT_VERSION=0.10.6
ENV GH_HOOKS_DIR /app/hooks

RUN ros install fukamachi/qlot/${QLOT_VERSION}

WORKDIR /app
COPY . /app

RUN qlot install && \
  qlot exec ros -e "(ql:quickload :github-webhook/server)"

EXPOSE 5000
ENTRYPOINT ["/app/docker/entrypoint.sh"]
