# Customer application maintainer guide

## Start here

These procedures are for geospatial data scientists maintaining skinned
customer applications derived from `ohwm2`. Positron or VS Code Source Control
is the normal interface. The guides provide copy/paste commands only where the
UI does not clearly express the two-repository workflow.

Begin with
[Working with a customer application repository](downstream-repository-basics.md).
It explains the local folder, customer `origin`, shared `upstream`, and safety
rules.

## Choose the task

| I need to... | Follow... |
|---|---|
| Create a customer repository for the first time | [Create a customer application repository](downstream-app-bootstrap.md) |
| Replace an existing unrelated customer app while retaining its repository | [Migrate an existing customer repository](downstream-legacy-repository-migration.md) |
| Bring a published `ohwm2` release into a customer app | [Update a customer application from an `ohwm2` release](downstream-skin-synchronization.md) |
| Check repository, release, skin, and asset state | [Check a customer application repository](downstream-verification.md) |
| Stage, promote, or roll back a customer app | [Stage, promote, and roll back](downstream-promotion-and-rollback.md) |
| Publish an existing customer app to Posit Connect Cloud | [Publish through Posit Connect Cloud](posit-connect-cloud-deployment.md) |
| Change only customer wording or images | Use the routine customer-change procedure below |
| Change calculations or shared Shiny behavior | Stop and create an `ohwm2` feature branch; do not patch one customer repo |

## Routine customer wording or image change

This is the workflow most similar to an ordinary small-team feature branch:

1. Open the customer repository, switch to `main`, and pull from `origin`.
2. Confirm Source Control is clean.
3. Create a feature branch such as `skin/update-results-guidance`.
4. Change only `inst/app/skin.yml` and files below
   `inst/app/www/customer/`.
5. Run and visually review the complete application.
6. Run tests and regenerate `manifest.json`.
7. Review, stage, and commit the intended files.
8. In a fresh R console, run:

   ```r
   pkgload::load_all()
   check_downstream_repository()
   ```

9. Publish the branch to `origin` and open a PR into customer `main`.
10. After review and merge, follow the staging and promotion guide.

If a requested change needs an R file, test, dependency, calculation, or
shared UI structure to differ, it is not a skin-only change. Move the request
to `ohwm2`.

## Team roles

No dedicated DevOps role is assumed. For the few higher-risk actions, use two
application maintainers:

- the operator runs the documented procedure;
- the reviewer confirms repository URLs, release tag, PR direction, and
  production commit/manifest evidence.

The reviewer does not need advanced Git skills; they compare the displayed
values with the approved release and customer repository.

Codex may execute the documented Git, dependency, validation, and evidence
steps when requested. The operator still performs scientific acceptance
testing and approves GitHub mutations and production publication. The
runbooks are the execution contract and audit trail; maintainers are not
expected to remember or manually reproduce every command.
