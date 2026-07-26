# Downstream application metadata schema

## Purpose

Each customer repository stores non-secret lineage and deployment inputs in
`.fluvial-app.yml`. The file tells maintainers and verification tooling what
application is being checked, which immutable `ohwm2` release it has approved,
where its skin lives, and how its manifest is generated.

This metadata describes expected state. Git and generated artifacts remain the
authoritative evidence of actual state.

## Version 1 contract

```yaml
schema_version: 1

application_id: floodplain-connectivity

upstream:
  repository: https://github.com/FluvialGeomorph/ohwm2.git
  remote: upstream
  release: "2026.07.25"

skin:
  file: inst/app/skin.yml
  schema_version: 1
  customer_assets: inst/app/www/customer

deployment:
  manifest: manifest.json
  dependency_resolution: library
```

## Field rules

### `schema_version`

- Required integer.
- Version 1 is the only accepted value.

### `application_id`

- Required string.
- Must match `^[a-z][a-z0-9-]*$`.
- Identifies the downstream product, not a deployment environment.
- Does not contain a customer name, credential, hostname, or secret.

### `upstream`

- `repository` is the canonical HTTPS Git URL for `ohwm2`.
- `remote` is the local Git remote name and must be `upstream` in version 1.
- `release` is a non-empty immutable Git tag that exists in the upstream
  repository.
- Verification must prove that the tagged commit is an ancestor of the
  downstream commit.

### `skin`

- `file` must be `inst/app/skin.yml`.
- `schema_version` must match the skin schema version used by the merged skin.
- `customer_assets` must be `inst/app/www/customer`.
- The skin and every referenced customer asset must exist before deployment.

### `deployment`

- `manifest` must be `manifest.json`.
- `dependency_resolution` is `library` for version 1 because the current
  `rsconnect` strict comparison rejects semantically equivalent R version
  strings. This field records an explicit, reviewable workaround.
- The manifest must be regenerated after application files or dependencies
  change.

## Invariants

- The file contains no credentials, tokens, platform account identifiers, or
  personally identifiable information.
- It does not select scientific calculations or Shiny workflow behavior.
- It does not duplicate the `fluvgeo` version; that requirement is derived from
  `DESCRIPTION`, `renv.lock`, and the generated manifest.
- It does not duplicate the downstream commit; that value is derived from Git.
- Unknown fields are rejected so misspellings cannot silently disable a check.

## Expected downstream-owned paths

Version 1 recognizes these expected downstream differences:

- `.fluvial-app.yml`;
- `inst/app/skin.yml`;
- files below `inst/app/www/customer/`;
- `manifest.json`;
- platform-generated deployment metadata ignored or explicitly documented by
  the downstream repository.

Changes outside this set require review. They are not automatically invalid,
but they cannot be treated as presentation-only divergence.
