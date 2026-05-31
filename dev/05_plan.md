# ohwm2 — Active Work Plan

_Last updated: 2026-05-31_

## Current milestone: Stabilize ohwm2 for production

## Current focus

Stabilize the Results workflow, preserve the working reactive fix, and begin a phased refactor toward a modular architecture.

## What was accomplished

- Resolved the silent reactive failure in `view_results`.
- Confirmed the root cause was reactive timing around programmatic slider updates.
- Fixed the issue by capturing slider values into local variables before calling `updateSliderInput()`.
- Restored reliable automatic navigation to the Results tab.
- Confirmed that plots and discharge tables now render successfully when the Results workflow completes.

## Immediate next steps

1. Keep the current working fix as the baseline.
2. Avoid reintroducing direct `isolate(input$...)` calls inside `updateSliderInput()` when the same observer also depends on those inputs.
3. Begin decomposing `app_server.R` into smaller units.
4. Identify Shiny modules for major workflow boundaries:
   - Draw XS workflow
   - Draw Flowline workflow
   - Results workflow
   - plots and tables
5. Move toward stable top-level renderers and clearer state boundaries.
6. Reduce the amount of hidden reactive coupling in the server logic.

## Architectural guidance

The current server structure has outgrown what is practical to maintain as a monolithic reactive file. The architectural decision and rationale are recorded in the applicable ADR in `dev/decisions/`.

Future work should follow that ADR and prioritize:
- modular boundaries
- explicit workflow state
- less shared mutable reactive state
- clearer separation of orchestration, computation, and rendering

## Near-term work items

- Review `app_server.R` for high-complexity observers.
- Identify the smallest safe extraction candidates for modules.
- Preserve current behavior while refactoring.
- Update the plan as the refactor is broken into concrete tasks.

## Definition of done for this phase

This phase is complete when:
- the current Results behavior remains stable,
- the app’s major workflows are decomposed into clearer units,
- and the reactive architecture is significantly easier to understand and maintain.
