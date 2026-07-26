# Stage, promote, and roll back a customer application

## Purpose

Use this procedure after a customer configuration or synchronization PR has
been merged. It records exactly what was tested and ensures production uses
the same customer commit and manifest as staging.

This procedure does not require changing Git remotes. All Git work is in the
customer repository, whose local name is `origin`.

## Before staging

In Positron or VS Code, open the customer repository, switch to `main`, pull
from `origin`, and confirm Source Control is clean.

Run the read-only preflight in the R console:

```r
pkgload::load_all()
check_downstream_repository()
```

Then record the exact evidence in the terminal:

```powershell
git rev-parse HEAD
git remote get-url origin
git remote get-url upstream
Get-FileHash -Algorithm SHA256 manifest.json
```

Copy the complete commit and manifest hash into the deployment record. Do not
use only a branch name such as `main`; branch names can move.

## Deployment record template

Store this record in the team's approved non-secret operational location:

```text
Application:
Environment:
Deployment date and time:
Downstream repository:
Downstream commit:
Upstream release:
R version:
Manifest SHA-256:
Deployment destination:
Interactive reviewer:
Previous production commit:
Previous manifest SHA-256:
Notes:
```

Do not include tokens, passwords, client secrets, or other credentials.

## Stage the application

1. Regenerate or confirm `manifest.json` for the recorded commit and
   dependency state.
2. Confirm `git status --short` prints nothing.
3. Deploy that commit and manifest to staging using the team's normal
   rsconnect procedure.
4. Start a fresh staging session.
5. Confirm customer title, instructions, theme, and images.
6. Complete Draw XS, Draw Flowline, and Results.
7. Record the staging destination and reviewer.
8. Obtain explicit approval to promote.

If any file changes during staging review, stop. Commit the change through a
new customer PR, regenerate the manifest, and repeat staging.

## Confirm the evidence before production

Immediately before production, rerun:

```powershell
git rev-parse HEAD
Get-FileHash -Algorithm SHA256 manifest.json
git status --short
```

The commit and hash must exactly match the staging record, and the status
command must print nothing. If they differ, do not deploy.

## Promote to production

1. Deploy the same recorded commit and manifest to production.
2. Run a concise production smoke test in a fresh session.
3. Complete the deployment record.
4. Preserve the previous production commit and manifest hash as the rollback
   target.

Promotion is a deployment action. Do not create a new Git commit, merge,
rebase, or move a tag during promotion.

## Roll back production

Rollback means redeploying the previous known-good commit and its matching
manifest. It does not mean undoing Git history.

1. Select the previous commit and manifest pair from the deployment record.
2. Confirm both identifiers with a second maintainer.
3. Redeploy that pair to production.
4. Run the production smoke test.
5. Record the rollback time, restored commit and hash, failed deployment, and
   observed problem.
6. Diagnose the issue through a normal `ohwm2` or customer feature branch and
   PR.

Do not move a release tag, Force Push, reset customer `main`, or create an
unreviewed code revert during an operational rollback.

## Stop and ask for help when

- Source Control is not clean;
- `origin` or `upstream` has an unexpected URL;
- the preflight fails;
- the commit or manifest hash differs from staging;
- the previous known-good commit and manifest are unknown;
- a credential or external service is unavailable;
- the customer workflow fails.
