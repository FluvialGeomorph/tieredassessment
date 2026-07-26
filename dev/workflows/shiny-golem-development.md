# Shiny and golem development

## Purpose

Use this workflow for changes to the `ohwm2` Shiny application. The app is an
R package built with `{golem}`; package quality, Shiny behavior, and deployment
readiness are one engineering surface.

## Repository-specific boundaries

- `run_app()` is the application entry point.
- `app_ui()` assembles the UI and `app_server()` coordinates server behavior.
- `fluvgeo` owns the core geomorphic computations. Keep this repository focused
  on application orchestration, state transitions, validation, and presentation.
- Prefer cohesive `mod_<feature>_ui()` and `mod_<feature>_server()` modules as
  workflow boundaries become stable enough to extract.
- Keep non-reactive transformations and validation in ordinary functions under
  `R/`; reactive code should orchestrate those functions.
- Preserve the current behavior while hardening one transition boundary at a
  time. Do not combine Draw XS, Draw Flowline, and Results refactors in one
  change set.

## Reactivity and state

- Make ownership and readiness of workflow state explicit.
- Keep observers small and focused on one transition or side effect.
- Capture reactive values in local variables before calling `update*Input()`.
- Avoid consuming and mutating the same input in one timing-sensitive path.
- Keep renderers stable rather than defining them inside large event observers
  when practical.
- Use narrow injectable seams when a critical side effect cannot otherwise be
  tested deterministically.
- Do not use mutable global state shared across Shiny sessions.

## Robustness and user experience

- Validate prerequisites before downstream computation with `req()`,
  `validate()`, `need()`, or an explicit helper contract.
- Gate outputs until their workflow state is ready.
- Provide deterministic first-run, repeat-run, navigation, reset, empty, and
  error behavior.
- Keep technical diagnostics out of user-facing messages.
- Avoid heavy computation inside render functions when it can be prepared,
  cached, or incrementally updated.

## Configuration and secrets

- Treat development, test, and production configuration as distinct.
- Never commit credentials or tokens.
- ArcGIS authentication uses `ARCGIS_CLIENTID`, `ARCGIS_CLIENTSECRET`, and
  `ARCGIS_HOST` from the runtime environment or deployment platform secret
  store.
- Confirm deployment target, authentication requirements, expected concurrency,
  connectivity constraints, and resource assumptions before changing deployment
  architecture.

## Testing workflow

Follow `dev/workflows/shiny-workflow-testing.md`. In brief:

1. Encode the observed or highest-risk behavior as a focused regression test.
2. Extract only the smallest state contract or seam needed for deterministic
   testing.
3. Preserve the default production path.
4. Test first-run, repeat-run, readiness-gate, and fresh-session behavior where
   the transition warrants it.
5. Run focused tests, the full test suite, and package checks when the configured
   R environment is available.

## Deployment readiness

Before deployment, verify:

- package tests and `R CMD check`;
- reproducible dependency state and the intended `fluvgeo` release;
- required environment variables and platform secrets;
- complete workflow smoke-test results;
- logging and monitoring expectations;
- CPU, memory, concurrency, and known bottleneck assumptions;
- the generated deployment manifest.

Record durable architecture changes in `dev/architecture/` or
`dev/decisions/`, and update this workflow when the repeatable process changes.
