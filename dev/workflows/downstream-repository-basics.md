# Working with a customer application repository

## Who this guide is for

This guide is for geospatial data scientists who use Positron or VS Code
Source Control for normal branch, commit, and push work. It explains the small
amount of Git terminology and terminal use required to keep a customer
application synchronized with `ohwm2`.

You do not need to become a Git administrator. The one-time setup should be
performed by the application maintainer, with a second team member reviewing
the repository URLs before the first push.

## The three places involved

| Name in these guides | Meaning | What you do there |
|---|---|---|
| Working folder | The repository folder on your computer | Edit files, run the app, create branches, and commit |
| `origin` | The customer application repository on GitHub | Push customer branches and open customer PRs |
| `upstream` | `https://github.com/FluvialGeomorph/ohwm2.git` | Fetch shared application releases; never push customer work here |

For a Floodplain Connectivity application, the arrangement looks like this:

```text
FluvialGeomorph/ohwm2                       customer repository
Git name: upstream                          Git name: origin
shared application releases  ---------->   Floodplain Connectivity app
                                             |
                                             +-- customer skin and assets
                                             +-- customer branches and PRs
```

`origin` and `upstream` are short names stored only in each local clone. They
are not branches. Renaming a remote does not rename a GitHub repository or
change any code.

Remote settings do not travel with commits or GitHub PRs. Each fresh clone on
each maintainer's computer must configure `upstream` and the push safeguards
once.

## Know which folder Positron is showing

Positron and VS Code Source Control show changes only for the repository folder
open in that window. They do not automatically combine sibling customer
repositories, the canonical `ohwm2` checkout, or a separate Git worktree.

Before reviewing or committing, run:

```powershell
git rev-parse --show-toplevel
git status --short --branch
```

The first command must print the folder you intend to change. The second must
print the expected branch. A committed or merged change will not appear as an
uncommitted Source Control change; use the repository history to review it.

When two Codex tasks are active, give each task a different repository or Git
worktree and open that exact folder in its own Positron window. Do not allow
two writing tasks to share one working folder.

## Configure a fresh clone on another computer

Use this only after the customer repository has been created. Clone the
customer repository normally through Positron, VS Code, or GitHub. It will
automatically be named `origin`.

Open the cloned customer folder and run:

```powershell
git remote -v
git remote add upstream https://github.com/FluvialGeomorph/ohwm2.git
git remote set-url --push upstream DISABLED
git config remote.pushDefault origin
git fetch upstream --tags
git remote -v
git config --get remote.pushDefault
```

Before the `remote add` command, the first `git remote -v` should show only the
customer repository as `origin`. After setup:

- `origin` fetch and push URLs are the customer repository;
- `upstream` fetch is canonical `ohwm2`;
- `upstream` push is `DISABLED`;
- the final command prints `origin`.

If `upstream` already exists, do not run `remote add` again. Review its current
URL using `git remote -v` and compare it with the expected result below.

## Normal Source Control work

After the one-time setup, use the Positron or VS Code Source Control UI for:

- creating and switching ordinary feature branches;
- viewing changed files;
- staging and committing changes;
- publishing a branch to `origin`;
- pulling the customer repository's `main` branch.

When the UI asks where to publish a customer branch, choose `origin`. Do not
choose `upstream`.

## Files a customer repository owns

Normal customer-only changes are limited to:

- `.fluvial-app.yml`;
- `inst/app/skin.yml`;
- files below `inst/app/www/customer/`;
- `manifest.json`;
- documented platform deployment metadata.

R code, tests, `inst/app/skin-default.yml`, `DESCRIPTION`, `renv.lock`, and
schemas are shared files supplied by `upstream`. If a customer request needs
one of those files to differ, make the change in `ohwm2` and publish a new
release.

## The few terminal commands you need

Open **Terminal > New Terminal** in Positron or VS Code. Confirm the prompt is
inside the intended customer repository before pasting a command.

### Show the repository you are in

```powershell
git rev-parse --show-toplevel
git status --short --branch
```

The first command prints the repository folder. The second prints the current
branch and any uncommitted files.

### Show where pushes and shared releases go

```powershell
git remote -v
git config --get remote.pushDefault
```

The expected result is:

```text
origin    https://github.com/FluvialGeomorph/<customer-repository>.git (fetch)
origin    https://github.com/FluvialGeomorph/<customer-repository>.git (push)
upstream  https://github.com/FluvialGeomorph/ohwm2.git (fetch)
upstream  DISABLED (push)
```

The disabled `upstream` push URL is an intentional safety guard. It prevents a
customer branch from being accidentally pushed to `ohwm2`. The default push
remote is also configured as `origin`, which the final command prints.

### Download shared release information

```powershell
git fetch upstream --tags
```

This downloads commits and tags into the local folder. It does not change the
current branch, customer files, or GitHub repositories.

## Safety rules

- Never use **Force Push**.
- Never push a customer branch to `upstream`.
- Never merge an untagged `upstream/main` into a customer application.
- Never rebase customer `main` or a long-lived customer branch.
- Never resolve a merge conflict by choosing "Accept All" without reviewing
  which repository owns each file.
- Never paste a Git command while unsure which repository folder is active.

If a command reports a merge conflict, authentication error, rejected push, or
unexpected URL, stop. Preserve the message and ask the `ohwm2` maintainer or a
second application maintainer to review it. Do not try force options.

## Where pull requests go

A customer synchronization PR stays entirely in the customer repository:

```text
customer sync/<release> branch  --->  customer main
```

It is not a PR from the customer repository into `FluvialGeomorph/ohwm2`.
Shared functional changes belong on a separate feature branch and PR in
`ohwm2`.
