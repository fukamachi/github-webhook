ARG SBCL_VERSION=2.0.9
FROM fukamachi/sbcl:${SBCL_VERSION} AS base
ARG QLOT_VERSION=0.10.6
ENV LANG C.UTF-8
ENV GH_HOOKS_DIR /app/hooks

RUN ros install fukamachi/qlot/${QLOT_VERSION}

WORKDIR /app
COPY . /app
RUN cat /app/docker/init.lisp >> ~/.roswell/init.lisp

RUN qlot install && \
  qlot exec ros -e "(ql:quickload :docker-gh-webhook)"

EXPOSE 5000
ENTRYPOINT ["/app/docker/entrypoint.sh"]
