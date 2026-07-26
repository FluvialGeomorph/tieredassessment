# Create a customer application repository

## Purpose and audience

Use this procedure once to create an OHWM, Floodplain Connectivity, or Tiered
Assessment repository from an approved `ohwm2` release. It assumes the
maintainer normally uses Positron or VS Code Source Control and can copy
reviewed commands into its terminal.

Read
[Working with a customer application repository](downstream-repository-basics.md)
first. In this procedure:

- `origin` is the customer repository you will push to;
- `upstream` is `FluvialGeomorph/ohwm2`, which supplies shared releases.

## Before starting

Obtain these values and have a second team member review them:

| Placeholder | Example |
|---|---|
| `<release>` | `2026.07.25` |
| `<folder>` | `floodplain-connectivity` |
| `<customer-repository-url>` | `https://github.com/FluvialGeomorph/floodplain-connectivity.git` |
| `<application-id>` | `floodplain-connectivity` |

Also obtain the approved skin text, customer assets, deployment destinations,
and platform secrets. Do not place secrets in Git.

The customer GitHub repository must already exist and must be empty. When
creating it, do not initialize it with a README, `.gitignore`, or license. If
the repository is not empty, stop rather than force-pushing over its history.

## Step 1: clone the approved release

Open a terminal in the parent folder where the application folder should be
created. Edit the two quoted values, then run the block:

```powershell
$appRelease = "2026.07.25"
$appFolder = "floodplain-connectivity"
git clone --branch $appRelease https://github.com/FluvialGeomorph/ohwm2.git $appFolder
Set-Location -LiteralPath $appFolder
git switch -c main
```

The clone initially calls `ohwm2` `origin`. The next step corrects that name
before any customer work is pushed.

## Step 2: assign the two repository roles

Edit the quoted customer URL, then run:

```powershell
$customerRepositoryUrl = "https://github.com/FluvialGeomorph/floodplain-connectivity.git"
git remote rename origin upstream
git remote add origin $customerRepositoryUrl
git remote set-url --push upstream DISABLED
git config remote.pushDefault origin
git remote -v
git config --get remote.pushDefault
```

Review the output character by character:

- `origin` must be the customer repository;
- the `upstream` fetch URL must be
  `https://github.com/FluvialGeomorph/ohwm2.git`;
- the `upstream` push URL must be `DISABLED`;
- the last command must print `origin`.

If either URL is wrong, stop before pushing. Do not improvise a correction or
use Force Push. Ask another maintainer to review the folder and URLs.

## Step 3: confirm the starting point

Run:

```powershell
git status --short --branch
git describe --tags --exact-match
```

Expected results:

- the current branch is `main`;
- the working tree has no changed files;
- the second command prints the approved `<release>`.

## Step 4: add the customer-owned files

In Positron or VS Code:

1. Create `.fluvial-app.yml` from
   [the downstream metadata example](../examples/downstream-app.yml).
2. Set `application_id` and `upstream.release`. Do not change the canonical
   repository URL, remote name, skin paths, manifest path, or dependency mode.
3. Copy the closest example skin to `inst/app/skin.yml`.
4. Replace example titles and instructions with approved customer text.
5. Place customer images below `inst/app/www/customer/` and reference them as
   `www/customer/<file-name>` in the skin.
6. Review Source Control. Only the paths listed under
   [Files a customer repository owns](downstream-repository-basics.md#files-a-customer-repository-owns)
   should differ.

## Step 5: run and review the application

In a fresh R console, restore the released dependency state:

```r
renv::restore()
renv::status()
```

Do not continue unless `renv::status()` reports no issues. Launch the app with:

```r
source("dev/scripts/run_dev.R")
```

Then:

1. Confirm the customer title, browser title, navigation labels, guidance,
   colors, and images.
2. Complete Draw XS, Draw Flowline, and Results.
3. Stop the app and run:

   ```r
   devtools::test()
   devtools::check()
   ```

4. Generate `manifest.json`:

   ```r
   rsconnect::writeManifest(dependencyResolution = "library")
   ```

Do not continue if scientific behavior differs from `ohwm2`; customer
functional code does not belong in the downstream repository.

## Step 6: create the first customer commit

Use Source Control to:

1. review every changed file;
2. stage the customer metadata, skin, assets, and manifest;
3. commit with a message such as `configure floodplain connectivity app`.

The working tree must be clean after the commit.

## Step 7: run the read-only preflight

In the R console:

```r
pkgload::load_all()
check_downstream_repository()
```

Success begins with:

```text
PASS: downstream repository preflight
```

An error is a stop condition. Use
[the preflight troubleshooting section](downstream-verification.md#troubleshooting);
do not bypass the check.

## Step 8: publish customer `main`

In Source Control, choose **Publish Branch**. If prompted for a remote, select
`origin`.

The equivalent terminal command is:

```powershell
git push --set-upstream origin main
```

Open the customer repository on GitHub and confirm its `main` branch contains
`.fluvial-app.yml`. Confirm no branch or commit was pushed to `ohwm2`.

## Step 9: stage and promote

Follow
[Stage, promote, and roll back](downstream-promotion-and-rollback.md).
Staging and production must use the same customer commit and manifest.

## Stop and ask for help when

- the customer GitHub repository is not empty;
- `git remote -v` shows unexpected URLs;
- the approved release tag cannot be found;
- Git reports unrelated histories, a rejected push, or a merge conflict;
- Source Control shows shared R code or tests as customer changes;
- a secret appears in Source Control;
- the preflight, tests, manifest generation, or interactive workflow fails.

Never solve these conditions with Force Push, rebase, `--force`, or by deleting
the local `.git` folder.

## Durable outputs

- customer repository with preserved `ohwm2` history;
- reviewed `origin` and `upstream` URLs;
- validated customer metadata, skin, assets, and manifest;
- staged and production deployment records;
- initial known-good rollback commit.
