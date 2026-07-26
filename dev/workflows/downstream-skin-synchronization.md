# Synchronizing a skinned downstream application

## Ownership boundary

The `ohwm2` upstream owns:

- R application code and tests;
- `inst/app/skin-default.yml`;
- the skin schema and loader;
- generic assets required by the default skin.

A downstream application owns:

- `.fluvial-app.yml`;
- `inst/app/skin.yml`;
- customer-specific assets under `inst/app/www/customer/`;
- its generated deployment manifest;
- deployment configuration selecting a non-packaged override, if used.

Downstream repositories should not customize `app_ui.R` or `app_server.R` for
branding or guidance.

## Initial setup

Follow `dev/workflows/downstream-app-bootstrap.md`. Do not create a downstream
repository through a mechanism that discards `ohwm2` Git ancestry.

## Periodic synchronization

### Inputs

- clean downstream `main`;
- target immutable upstream release;
- current `.fluvial-app.yml`;
- reviewed release and migration notes.

### Ordered actions

1. Create `sync/<release>` from downstream `main`.
2. Fetch the `upstream` remote and verify the intended tag exists.
3. Merge the intended upstream release tag. Do not rebase long-lived
   downstream history.
4. Stop on conflicts in upstream-owned paths until ownership and intent are
   understood.
5. Preserve the downstream-owned skin, customer assets, and metadata.
6. Update `.fluvial-app.yml` to record the merged upstream release.
7. Review changes to `skin-default.yml`, `dev/schemas/app-skin.md`,
   `DESCRIPTION`, `renv.lock`, and release migration notes.
8. Hydrate or restore dependencies and require `renv::status()` to report a
   consistent project.
9. Run `ohwm2::validate_app_skin_file("inst/app/skin.yml")`, focused skin
   tests, the full package tests, `R CMD check`, and the normal Shiny smoke
   test.
10. Verify the customer title, navigation labels, instructions, assets, and
    complete workflow.
11. Regenerate the manifest using the dependency-resolution mode recorded in
    `.fluvial-app.yml`.
12. Run the current preflight in `downstream-verification.md`, followed by its
    listed manual checks. Release mode is planned but not yet implemented.
13. Open and review the synchronization pull request.
14. Follow `dev/workflows/downstream-promotion-and-rollback.md`.

### Stop conditions

Stop synchronization when:

- the target release is not immutable or is not from the configured upstream;
- the merge introduces unexplained shared-code divergence;
- the skin schema requires an undocumented migration;
- dependencies, tests, workflow behavior, or manifest generation fail;
- staging cannot use the exact proposed production commit.

### Durable outputs

- merge commit retaining upstream ancestry;
- updated downstream metadata and manifest;
- verification report;
- staged and promoted downstream release record.

## Schema changes

New optional presentation fields should normally be supplied by
`skin-default.yml`, allowing downstream skins to inherit them. A schema-version
change requires explicit downstream migration before deployment.

If an upstream change requires customer-specific functional behavior, stop the
sync and make an architecture decision. That change is outside the skin
boundary.

See ADR 0005 and `dev/schemas/downstream-app-metadata.md` for the repository and
metadata contracts.
