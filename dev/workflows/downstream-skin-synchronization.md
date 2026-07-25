# Synchronizing a skinned downstream application

## Ownership boundary

The `ohwm2` upstream owns:

- R application code and tests;
- `inst/app/skin-default.yml`;
- the skin schema and loader;
- generic assets required by the default skin.

A downstream application owns:

- `inst/app/skin.yml`;
- customer-specific assets under `inst/app/www/`;
- deployment configuration selecting a non-packaged override, if used.

Downstream repositories should not customize `app_ui.R` or `app_server.R` for
branding or guidance.

## Initial setup

1. Add `ohwm2` as an upstream Git remote.
2. Create `inst/app/skin.yml` with only values that differ from the defaults.
3. Add referenced favicon or future branding assets below `inst/app/www/`.
4. Run the skin tests and launch the application locally.
5. Record any functional differences separately; do not encode them as
   undocumented skin fields.

## Periodic synchronization

1. Start from a clean downstream feature branch.
2. Fetch the `ohwm2` upstream.
3. Merge or rebase the intended upstream release according to the downstream
   repository's Git policy.
4. Resolve conflicts while preserving the downstream-owned `skin.yml` and
   customer assets.
5. Review changes to `skin-default.yml` and `dev/schemas/app-skin.md`.
6. Run focused skin tests, the full package tests, and the normal Shiny smoke
   test.
7. Verify the customer title, navigation labels, instructions, assets, and
   complete workflow before deployment.

## Schema changes

New optional presentation fields should normally be supplied by
`skin-default.yml`, allowing downstream skins to inherit them. A schema-version
change requires explicit downstream migration before deployment.

If an upstream change requires customer-specific functional behavior, stop the
sync and make an architecture decision. That change is outside the skin
boundary.
