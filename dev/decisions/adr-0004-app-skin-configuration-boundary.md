# ADR 0004: Make `ohwm2` the skinnable application template

- **Status:** Accepted
- **Date:** 2026-07-25
- **Deciders:** Maintainers of `FluvialGeomorph/ohwm2`
- **Supersedes:** ADR 0001 decision 2, which named `ohwm` as the canonical
  upstream template

## Context

The OHWM, Floodplain Connectivity, and Tiered Assessment applications need the
same generic fluvial geomorphic workflow but different branding, terminology,
and task-oriented user guidance. Copying `app_ui.R` and `app_server.R` into each
repository would make routine synchronization of structural and reliability
improvements increasingly difficult.

Presentation content was previously hard-coded across the UI and server.
Navigation also used visible labels as reactive identifiers, so merely changing
a tab label could break programmatic navigation.

## Decision

`ohwm2` is the canonical template for the shared Shiny application.

The app will have a versioned, presentation-only skin contract:

- `inst/app/skin-default.yml` is complete and template-owned.
- A downstream `inst/app/skin.yml` is an optional partial override.
- `FLUVIAL_APP_SKIN_FILE` or `run_app(skin_file = ...)` may select another
  override at deployment or startup.
- Defaults and overrides are merged and validated before the app is built.
- The normalized skin is injected into UI and server construction.
- Internal workflow identifiers remain stable and independent of visible
  labels.

Skin configuration may change identity, theme, assets, labels, and guidance. It
must not control calculations, reactive transition behavior, validation rules,
or stage availability.

## Rationale

Separating template-owned defaults from downstream overrides gives the template
room to add presentation fields without requiring each derived repository to
copy a complete file. It also concentrates expected downstream differences in
one file and its referenced assets, reducing merge conflicts during upstream
synchronization.

Loading and validating the skin once makes failures deterministic at startup
and avoids introducing configuration reads into reactive paths.

## Consequences

### Positive

- Derived applications can preserve customer guidance without forking core UI
  and server files.
- Visible navigation terminology can vary safely.
- New default fields can be inherited by existing downstream skins.
- Invalid or stale configuration fails with a targeted startup error.
- Structural updates can be synchronized with a smaller expected conflict
  surface.

### Costs and constraints

- The skin schema becomes a maintained interface.
- Downstream assets must be packaged under `inst/app/www/`.
- Schema changes require compatibility and migration consideration.
- Customer-specific functional differences still require explicit architecture
  decisions; they cannot be hidden in skin configuration.

## Follow-up

- Keep initial extraction limited to identity, theme, workflow labels,
  instructions, buttons, progress messages, and favicon.
- Add richer guidance structures only when a concrete customer requirement
  justifies them.
- Keep technical results terminology in core code until it is shown to vary by
  task without changing semantics.
