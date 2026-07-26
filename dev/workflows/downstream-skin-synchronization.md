# Update a customer application from an `ohwm2` release

## Purpose and audience

Use this procedure to bring a published `ohwm2` release into an existing
customer application. Normal branch, commit, and push work uses Positron or
VS Code Source Control. A few reviewed terminal commands fetch and merge the
specific release.

Read
[Working with a customer application repository](downstream-repository-basics.md)
first. Remember:

- `origin` is the customer repository and receives your branch;
- `upstream` is `FluvialGeomorph/ohwm2` and supplies the release;
- the PR is opened in the customer repository, not in `ohwm2`.

## Ownership boundary

The customer repository may normally change only:

- `.fluvial-app.yml`;
- `inst/app/skin.yml`;
- files below `inst/app/www/customer/`;
- `manifest.json`;
- documented platform deployment metadata.

Shared R code, tests, `inst/app/skin-default.yml`, schemas, `DESCRIPTION`,
`renv.lock`, and renv activation files come from `upstream`. Do not edit these
files to resolve a customer preference.

## Before starting

Obtain the exact published `<release>` tag, such as `2026.08.15`, and review
the release and migration notes.

In Positron or VS Code:

1. Open the customer application folder.
2. Switch to `main`.
3. Pull from `origin`.
4. Confirm Source Control shows no changed files.

Then run:

```powershell
git rev-parse --show-toplevel
git status --short --branch
git remote -v
git config --get remote.pushDefault
```

Stop unless the folder, clean `main` branch, and remote URLs are exactly what
you expect and the final command prints `origin`. If a fresh clone has no
`upstream`, follow
[Configure a fresh clone on another computer](downstream-repository-basics.md#configure-a-fresh-clone-on-another-computer)
before creating the sync branch.

## Step 1: create the synchronization branch

Use Source Control to create a branch named:

```text
sync/<release>
```

Example: `sync/2026.08.15`.

Confirm the status bar shows the sync branch before continuing. Never perform
the release merge directly on customer `main`.

## Step 2: fetch the published release

In the terminal:

```powershell
$appRelease = "2026.08.15"
git fetch upstream --tags
git tag --list $appRelease
git show --no-patch --decorate $appRelease
```

The tag-list command must print the exact `<release>`. The show command lets
you review the tagged commit. If the tag is missing or unexpected, stop.

Fetching is safe: it downloads information but does not modify the current
branch or customer files.

## Step 3: merge the release

Confirm once more that the status bar shows `sync/<release>` and Source
Control is clean. Then run:

```powershell
$appRelease = "2026.08.15"
git merge --no-ff $appRelease
```

This creates a merge that preserves the relationship between the customer
application and the shared release.

### If the merge succeeds

Continue to Step 4. Source Control will show the shared release changes.

### If Git reports conflicts

Stop before choosing any conflict-resolution buttons.

1. Copy the complete terminal message into an issue or team note.
2. In Source Control, record the files under **Merge Changes**.
3. Ask the `ohwm2` maintainer and a customer-app maintainer to decide the
   correct resolution.

Do not use **Accept All Current**, **Accept All Incoming**, rebase, Force Push,
or another merge command. If the reviewed decision is to abandon the attempt,
the maintainer can use:

```powershell
git merge --abort
```

That command returns the sync branch to its pre-merge state.

## Step 4: update customer metadata

In `.fluvial-app.yml`, change only:

```yaml
upstream:
  release: "<release>"
```

Preserve the customer skin and assets. Review changes to:

- `inst/app/skin-default.yml`;
- `dev/schemas/app-skin.md`;
- `DESCRIPTION`;
- `renv.lock`;
- release and migration notes.

If the skin schema changed, complete the documented migration. Do not guess at
new fields.

## Step 5: restore dependencies and test

In a fresh R console:

```r
renv::restore()
renv::status()
```

Require `renv::status()` to report no issues. Then run:

```r
devtools::test()
devtools::check()
source("dev/scripts/run_dev.R")
```

In the running app:

1. confirm customer titles, labels, instructions, theme, and images;
2. complete Draw XS, Draw Flowline, and Results.

Stop if scientific behavior fails or a customer-specific code patch appears
necessary. Fix shared behavior in `ohwm2`, publish a new release, and restart
this workflow with that release.

## Step 6: regenerate the manifest

Generate `manifest.json` with the mode recorded in `.fluvial-app.yml`. For
schema version 1:

```r
rsconnect::writeManifest(dependencyResolution = "library")
```

Review Source Control to confirm no secrets or unexpected files were added.

## Step 7: commit the downstream updates

Use Source Control to review, stage, and commit the metadata and regenerated
manifest. Use a message such as:

```text
synchronize ohwm2 release <release>
```

The upstream merge commit may already be present. A second commit for metadata
and the manifest is expected and keeps those downstream decisions visible.

## Step 8: run the preflight

The repository must be clean. In the R console:

```r
pkgload::load_all()
check_downstream_repository()
```

Require the printed PASS summary, then complete the remaining manual checks in
[Check a customer application repository](downstream-verification.md).

## Step 9: publish and open the customer PR

In Source Control, choose **Publish Branch**. If prompted, select `origin`.

Equivalent terminal command:

```powershell
$syncBranch = "sync/2026.08.15"
git push --set-upstream origin $syncBranch
```

Open a PR in the customer repository with:

- base branch: `main`;
- compare branch: `sync/<release>`.

The PR is not opened against `FluvialGeomorph/ohwm2`. Include the release tag,
preflight result, tests, interactive review, and manifest-generation result in
the PR description.

## Step 10: after the customer PR is merged

In Source Control:

1. switch to customer `main`;
2. pull from `origin`;
3. confirm the working tree is clean;
4. rerun `check_downstream_repository()`;
5. follow
   [Stage, promote, and roll back](downstream-promotion-and-rollback.md).

## Stop and ask for help when

- `origin` or `upstream` points to an unexpected URL;
- the release tag is missing;
- the merge is being attempted on `main`;
- Git reports conflicts, unrelated histories, or a rejected push;
- shared files contain unexplained customer-only changes;
- the skin migration, renv state, tests, app workflow, or manifest fails;
- staging cannot use the exact PR merge commit and manifest.

Never use Force Push or rebase to get around a stop condition.
