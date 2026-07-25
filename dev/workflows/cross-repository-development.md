# Cross-repository development

## Ownership boundary

`ohwm2` owns the Shiny application experience: workflow orchestration, reactive
state, input validation, output readiness, navigation, and presentation.
`fluvgeo` owns reusable geomorphic calculations and domain operations.

## Routing a change

- Change only `ohwm2` when the behavior is application-specific or concerns
  Shiny orchestration.
- Propose a `fluvgeo` change when domain logic is reusable outside this app.
- If both repositories must change, define the interface and compatibility order
  before editing either repository. Keep each repository independently
  reviewable and test its side of the contract.
- Do not duplicate a reusable domain fix in app code merely to avoid an explicit
  cross-repository dependency.

## Current lineage

`ohwm2` is the active stabilization target. The related `ohwm` app remains
frozen until improvements are deliberately backported. Do not broaden a focused
`ohwm2` task into template alignment or backport work without explicit scope.
