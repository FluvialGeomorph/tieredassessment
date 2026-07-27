# Migrate an existing customer repository to the shared application

## Purpose

Use this one-time procedure when an existing customer repository must retain
its GitHub URL and legacy history but does not share ancestry with `ohwm2`.
After this migration, use the normal downstream synchronization workflow.

This procedure applies to:

- `FluvialGeomorph/ohwm`;
- `FluvialGeomorph/floodplainconnectivity`;
- `FluvialGeomorph/tieredassessment`.

The approved migration carries no legacy code, assets, configuration, or
deployment files into the new application line.

## Required inputs

- an immutable, published `ohwm2` release tag;
- a clean local customer clone with reviewed `origin` and `upstream` URLs;
- the complete commit ID of legacy `origin/main`;
- an approved archival branch name such as
  `legacy-main-YYYY-MM-DD`;
- a verified recovery copy or complete Git bundle of all legacy branches and
  tags;
- approved customer skin text and assets;
- a second maintainer for the GitHub branch and default-branch checkpoints.

Do not begin from untagged `upstream/main`.

## Prepare without changing GitHub

From the customer repository:

```powershell
git status --short --branch
git remote -v
git config --get remote.pushDefault
git fetch --prune origin
git fetch --prune --tags upstream
git rev-parse origin/main
git fsck --full --strict
```

Confirm:

- Source Control is clean;
- `origin` is the customer repository;
- `upstream` fetches from `FluvialGeomorph/ohwm2` and has push URL
  `DISABLED`;
- the recorded legacy commit matches `origin/main`;
- the selected release tag resolves locally;
- the recovery copy includes every origin branch and legacy release tag.

A Git bundle is an optional additional recovery file. It is not the permanent
archive and must not be committed to `ohwm2` or a customer repository.

## Reviewed GitHub cutover

The following actions change shared GitHub state. The operator and reviewer
must confirm the repository URL, legacy commit, release tag, and archival
branch name before proceeding.

1. In GitHub, rename the existing default branch from `main` to the approved
   archival name. Do not delete it.
2. Confirm the renamed branch still resolves to the recorded legacy commit.
3. Refresh the local clone:

   ```powershell
   git fetch --prune origin
   git branch -m main <archival-branch>
   git branch --set-upstream-to=origin/<archival-branch> <archival-branch>
   ```

4. Create the new application line from the published release:

   ```powershell
   git switch -c main <release>
   ```

5. Configure `.fluvial-app.yml`, `inst/app/skin.yml`, customer assets, and the
   manifest using the normal bootstrap procedure.
6. Commit, run the complete downstream preflight, and obtain review.
7. Publish the new `main` to customer `origin`:

   ```powershell
   git push --set-upstream origin main
   ```

8. In GitHub, make the new `main` the default branch and apply the approved
   protections or rulesets.
9. Confirm the archival branch remains unchanged and visible.

Replace every placeholder before running a command. Never paste angle-bracket
placeholders literally.

## Verification

Before deployment, demonstrate:

- the archival branch equals the recorded legacy commit;
- new `main` contains the selected `ohwm2` release;
- no merge joined the unrelated histories;
- `.fluvial-app.yml` records the selected release;
- only customer-owned files differ from that release;
- `renv::status()`, tests, package check, interactive workflow, manifest, and
  `check_downstream_repository()` all pass;
- GitHub reports new `main` as the default branch.

Then follow the Posit Connect Cloud deployment procedure.

## Stop conditions

Stop without force-pushing, rebasing, or deleting a branch when:

- any repository URL, branch name, commit, or tag differs from the reviewed
  value;
- the customer worktree is not clean;
- the recovery copy is incomplete or cannot be verified;
- Git reports unrelated histories or a rejected push;
- a customer-owned file contains a secret;
- the new application requires a legacy file;
- validation or interactive testing fails;
- GitHub branch protection, rulesets, webhooks, or deployment integration
  behavior is not understood.

## Durable outputs

- unchanged archival branch containing the complete legacy line;
- new customer `main` descended from the immutable `ohwm2` release;
- reviewed customer configuration and manifest;
- recorded legacy and new commit IDs;
- validated Connect Cloud deployment using the existing content item and URL.
