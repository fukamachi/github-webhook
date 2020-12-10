# github-webhook

Docker container to listen for GitHub webhook events.

* https://hub.docker.com/r/fukamachi/github-webhook

## Usage

```shell
$ docker run -it -v ${PWD}/hooks:/app/hooks -p 5000:5000 fukamachi/github-webhook
```

`github-webhook` server will invoke scripts under `/app/hooks` when getting webhook requests.

## Hooks directory structure

`github-webhook` determines which hook scripts to run by its subdirectories. (eg. `hooks/<event>/<action>/*`)

The structure is something like this:
```
+ hooks/
  + push/
    - run-when-pushed.sh
  + issue/
    - run-on-all-issue-events.sh
    + created/
      - run-when-issue-is-created.sh
  + package/
    - run-on-all-package-events.sh
    + published/
      - run-when-package-is-published.sh
```

## Configurable variables

* `GH_HOOKS_DIR`: Directory path of hook scripts (Default: `/app/hooks`)
* `GH_SECRET`: GitHub Webhook secret token (Optional)
  * Skip the verification when this is not set
  * ref. [Securing your webhooks - GitHub Docs](https://docs.github.com/ja/free-pro-team@latest/developers/webhooks-and-events/securing-your-webhooks)

### Examples

```shell
# Using custom location for hook scripts
$ docker run -it -v ${PWD}/hooks:/code/hook-scripts -p 5000:5000 -e GH_HOOKS_DIR=/code/hook-scripts fukamachi/github-webhook

# Using secret token to refuse requests from other than GitHub
$ docker run -it -v ${PWD}/hooks:/app/hooks -p 5000:5000 -e GH_SECRET=xxxxxxxxxxxxxxxxxxx fukamachi/github-webhook
```

## Writing Hooks

### Available environment variables

| name                     | description                | example                   |
|--------------------------|----------------------------           |---------------------------|
| `GH_HOOK_EVENT_NAME`       | Event name of the webhook. | `push`, `issue`, `package`      |
| `GH_HOOK_EVENT_PATH`       | File path to store the whole payload. | `/tmp/event-xxxxxx.json` |
| `GH_HOOK_ACTION`           | Action name of the webhook. | `created`, `published` |
| `GH_HOOK_SENDER`           | User name that triggered the event. | `fukamachi` |
| `GH_HOOK_REPOSITORY`       | Repository full name. | `fukamachi/clack` |
| `GH_HOOK_REPOSITORY_OWNER` | Repository owner name. | `fukamachi` |
| `GH_HOOK_REPOSITORY_NAME`  | Repository name. | `clack` |
| `GH_HOOK_ORGANIZATION`     | Organization name of the event. | `pokepay` |
| `GH_HOOK_REF`              | Git reference. | `1da31df21ef7a87a296c954f59a9154b99f4f21f` |
| `GH_HOOK_BRANCH`           | Git branch name. This is an empty string if the event isn't related to a branch. | `master`, `production` |
| `GH_HOOK_TAG`              | Git tag name. This is an empty string if the event isn't related to a tag | `v1.0.0` |
| `GH_HOOK_PACKAGE_NAME`     | Package name. This is an empty string unless the event is `package`. | `hello-world` |
| `GH_HOOK_PACKAGE_TYPE`     | Package type. This is an empty string unless the event is `package`. | `npm` |
| `GH_HOOK_PACKAGE_VERSION`  | Package version. This is an empty string unless the event is `package`. | `1.0.0` |

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
