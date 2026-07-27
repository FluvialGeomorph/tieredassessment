# Application skin schema

## Purpose

The application skin is a presentation-only configuration contract. It allows
downstream applications to vary identity, theme, workflow labels, and guidance
without modifying shared geomorphic functionality or Shiny workflow behavior.

The complete template-owned defaults are stored in
`inst/app/skin-default.yml`. A downstream repository may add a partial override
at `inst/app/skin.yml` or provide its path through `FLUVIAL_APP_SKIN_FILE`.

## Merge and validation rules

- Both files use a top-level `default` configuration profile.
- The downstream mapping is recursively merged over the complete defaults.
- Sequences, including instruction lists, replace the default sequence.
- Missing downstream values inherit template defaults.
- Version 1 skins created before `workflow.results.progress_message` inherit
  `Preparing Slope and Discharge Data` during normalization.
- Unknown fields are rejected.
- Missing required fields after merging are rejected.
- Empty text values are rejected.
- The merged configuration is loaded once when `run_app()` constructs the app.
- Configuration values are data only; R expressions and arbitrary HTML are not
  part of this contract.

## Version 1 contract

```yaml
default:
  schema_version: 1

  identity:
    app_title: non-empty string
    browser_title: non-empty string
    favicon: www/path-to-packaged-file.png

  theme:
    bootswatch: non-empty bslib Bootswatch theme name
    version: 3 | 4 | 5

  workflow:
    draw_xs:
      nav_label: non-empty string
      sidebar_title: non-empty string
      instructions:
        - one or more non-empty plain-text items
      next_button: non-empty string
      progress_message: non-empty string

    draw_flowline:
      nav_label: non-empty string
      sidebar_title: non-empty string
      instructions:
        - one or more non-empty plain-text items
      next_button: non-empty string
      progress_message: non-empty string

    results:
      nav_label: non-empty string
      progress_message: non-empty string
```

## Invariants

- Internal navigation values are fixed as `draw_xs`, `draw_flowline`, and
  `results`; skin labels never become workflow identifiers.
- `favicon` must begin with `www/` and resolve to a file under
  `inst/app/www/`.
- A skin must not select computations, change validation rules, skip workflow
  stages, or enable customer-specific server branches.
- Schema evolution must be backward compatible through defaults or accompanied
  by a schema-version change and migration guidance.
