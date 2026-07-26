# Check a customer application repository

## What this check does

The preflight is a read-only safety check. It does not fetch, merge, change a
remote, commit, push, generate a manifest, or deploy.

Run it after committing the customer work, when Source Control shows no
changed files.

## Before running it

Open the customer application folder in Positron or VS Code. In the terminal:

```powershell
git status --short --branch
git remote -v
```

Confirm the intended branch, a clean working tree, customer `origin`, and
canonical `ohwm2` `upstream`. If the recorded release has not been fetched in
this clone, run:

```powershell
git fetch upstream --tags
```

Fetching downloads release information but does not modify the current branch.

## Run the operator-friendly check

In a fresh R console:

```r
pkgload::load_all()
evidence <- check_downstream_repository()
```

A successful check prints:

```text
PASS: downstream repository preflight
  Application: floodplain-connectivity
  Customer repository: https://github.com/FluvialGeomorph/floodplain-connectivity.git
  Upstream release: 2026.07.25
  Downstream commit: <complete Git commit>
  Skin schema: 1
  Referenced customer assets: 1
```

Keep the full commit from this output with staging and production records.
Advanced tooling can use the returned `evidence` list without parsing the
printed text.

## What PASS confirms

- `.fluvial-app.yml` uses the supported schema and exact reserved paths;
- Source Control is clean;
- `.fluvial-app.yml` is tracked at the repository root;
- `origin` exists, uses one reviewed fetch/push URL, and is the default push
  destination;
- `upstream` has the canonical `ohwm2` URL;
- pushes to `upstream` are disabled;
- the recorded release tag exists in the local clone;
- the release commit is an ancestor of the customer commit;
- changes since that release stay within customer-owned paths;
- the tracked customer skin merges with template defaults;
- skin schema versions agree;
- referenced customer assets are safe, present, and tracked.

PASS is necessary but is not yet permission to deploy.

## Checks that remain manual

- visually review customer guidance, branding, and assets;
- require `renv::status()` to report no issues and review dependency sources;
- run focused tests, the full test suite, and the normal package check;
- complete the interactive Draw XS through Results workflow;
- regenerate and review the manifest for the exact commit;
- stage, obtain approval, promote, smoke test, and record the deployment.

## Troubleshooting

Do not bypass a failed check. Read the first error and use this table.

| Error includes | What it usually means | Safe next action |
|---|---|---|
| `does not exist` for `.fluvial-app.yml` | Wrong folder or missing metadata | Run `git rev-parse --show-toplevel`; open the correct customer repository |
| `clean working tree` | Source Control has committed or uncommitted differences | Review the listed files; commit intended work or discard it through normal reviewed UI actions |
| `repository-root` or `is tracked` | Metadata, skin, or asset is misplaced or ignored | Move it to the documented path and commit it |
| remote `upstream` `must resolve to` | The clone points to the wrong shared repository | Stop before fetching or pushing; have a second maintainer review `git remote -v` |
| `protected push URL` or `remote.pushDefault` | The one-time push safeguards are missing | Return to the remote setup commands in the bootstrap guide; have a second maintainer review the result |
| remote `origin` | The customer repository is missing, wrong, or configured differently for fetch and push | Stop before pushing; compare `git remote -v` with the approved customer URL |
| `resolve upstream release` | The tag was not fetched or the release value is wrong | Run `git fetch upstream --tags`; compare `.fluvial-app.yml` with the published release |
| `not an ancestor` | The release was recorded without being merged | Return to the synchronization workflow; do not edit history or force the check |
| `outside owned paths` | Customer-only changes exist in shared code or tests | Move the functional change to an `ohwm2` feature branch or request maintainer review |
| skin schema or unknown field | The customer skin does not match the supported schema | Review `dev/schemas/app-skin.md` and the release migration notes |
| missing or unsafe asset | A skin image path is wrong, untracked, or leaves the customer folder | Correct the `www/customer/...` reference, add the file, and commit |

For Git conflict, rejected-push, authentication, or unrelated-history errors,
preserve the complete message and ask another maintainer for help. Never add
`--force`, rebase, delete `.git`, or change a remote merely to make the check
pass.

## Machine-readable validator

Automation can call the silent validator directly:

```r
evidence <- validate_downstream_repository(
  file = ".fluvial-app.yml",
  repository = "."
)
```

`check_downstream_repository()` calls this same validator and adds only the
plain-language PASS summary.

## Planned tooling

Later read-only increments will:

1. report dependency and manifest evidence;
2. represent each check in a stable machine-readable report;
3. add a command-line entry point suitable for CI;
4. add release mode after all required checks have deterministic evidence.

They will not fetch, merge, tag, resolve conflicts, generate manifests, deploy,
or promote releases.
