# Bootstrapping a downstream customer application

## Trigger

Use this workflow once for each customer application after an approved
`ohwm2` release exists.

## Inputs

- downstream repository URL;
- application ID;
- approved upstream release;
- reviewed customer skin copy;
- customer favicon and other presentation assets;
- staging and production deployment destinations;
- platform secrets supplied outside Git.

## Ordered actions

1. Clone `ohwm2` at the approved release while preserving its Git history.
   Do not use a template-generation operation that discards ancestry.
2. Rename the canonical repository remote to `upstream`.
3. Add the customer repository as `origin`.
4. Create `.fluvial-app.yml` using
   `dev/schemas/downstream-app-metadata.md`.
5. Copy the relevant example skin to `inst/app/skin.yml` and replace example
   text with approved customer copy.
6. Store customer assets below `inst/app/www/customer/` and update skin paths.
7. Configure platform secrets and deployment destinations outside Git.
8. Validate the skin, run package tests, and exercise the complete workflow.
9. Generate a manifest using the dependency-resolution mode recorded in
   `.fluvial-app.yml`.
10. Commit the downstream-owned files and push the initial downstream `main`.
11. Deploy the exact commit to staging, review it, then explicitly promote that
    same commit to production.

## Stop conditions

Stop bootstrap when:

- Git ancestry with the approved upstream release is absent;
- `origin` or `upstream` points at the wrong repository;
- customer code changes extend outside the expected downstream-owned paths;
- skin validation or a referenced asset fails;
- a secret appears in a tracked file;
- staging would use a different commit from the proposed production commit.

## Verification

Confirm:

- the approved upstream tag is an ancestor of downstream `HEAD`;
- `.fluvial-app.yml` and the skin conform to their schemas;
- all dependency sources are reproducible;
- the customer title, guidance, theme, and assets render correctly;
- Draw XS, Draw Flowline, and Results complete in a fresh session;
- rollback can identify the previous known-good commit.

## Durable outputs

- customer repository with preserved upstream ancestry;
- reviewed downstream metadata, skin, and assets;
- staged and production deployment records;
- initial known-good rollback commit.
