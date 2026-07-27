# 10 Design

## Overview

This app’s core engineering computations are provided by `fluvgeo`.  
The primary design challenge in this repository is Shiny orchestration: reactive workflow transitions, initialization order, and output readiness gating.

Recent reliability work focused on the Results workflow boundary, where reactive sequencing had produced silent failures and inconsistent run behavior.

The current design direction is:

1. preserve working behavior,
2. improve robustness at critical transition boundaries,
3. expose narrow orchestration seams for deterministic testing,
4. continue incremental modularization with regression protection in place,
5. keep task-specific presentation outside shared workflow implementation.

## Application skin boundary

`ohwm2` is the canonical template for the shared fluvial geomorphic
application. Task-specific identity and user guidance are supplied through a
versioned application skin rather than edits to `app_ui.R` or `app_server.R`.

The template owns a complete default skin. Downstream applications own only a
partial override and their customer-specific assets. The merged skin is
validated and injected when the Shiny app is constructed; it is not read from
reactive code.

Stable internal navigation values (`draw_xs`, `draw_flowline`, and `results`)
separate workflow identity from configurable labels. Skin configuration does
not own calculations, stage availability, reactive transitions, or validation
rules.

See ADR 0004, `dev/features/app-skinning.md`, and
`dev/schemas/app-skin.md`.

## Editable workflow state

Raw editor geometry and processed geomorphic outputs have separate ownership.
Each Draw Flowline submission snapshots the current XS editor state and creates
a generation-specific Flowline editor after terrain validation. Each Results
submission snapshots that active flowline and recomputes from the raw XS
snapshot. Previously processed cross sections are never reused as raw input,
and Shiny module IDs are never recreated for a later terrain generation.

See `dev/features/editable-workflow-and-dem-guardrails.md`.

## Optional watershed enrichment boundary

The OHWM Results workflow uses
`fluvgeo::cross_section(..., watershed = "skip")`. Watershed delineation is a
remote enrichment and is not required for the app's DEM-derived cross-section
geometry, water-surface volumes, or plots.

The cross-section contract still carries numeric
`Watershed_Area_SqMile`; its value is `NA_real_` in this workflow. Results table
helpers use `fluvgeo::xs_geometry()` directly and omit the Drainage Area row
when no scientifically valid value is available. The app does not fabricate a
replacement drainage area.

This requires `fluvgeo >= 2026.07.25.9000`. Release and deploy the backend
change before deploying this client change.

## Optional USGS reach-slope enrichment boundary

Manning discharge needs a positive reach-slope proxy, but completing the
Results workflow does not require the USGS NLDI/NHDPlus services. The preferred
USGS lookup runs after the first Results response is flushed, with bounded
request time and retry/backoff. Its structured result records source, status,
reason, and attempts.

The Results transition computes and caches three slope scales. **USGS Reach**
and **Sampled DEM Reach** are each one reach-wide value reused for every cross
section. **Local XS Neighborhood** is a complete profile keyed by
cross-section sequence and is also computed in one pass. Reactive changes to
cross-section selection, REM elevations, and Manning coefficients reuse these
caches and never repeat slope-profile work or contact USGS.

The user-facing slope-scale contract defaults to **USGS Reach**. **Sampled DEM
Reach** divides the elevation range of the same flowline points shown in the
longitudinal profile by their profile length. **Local XS Neighborhood** is the
signed slope centered at the selected cross section from adjacent thalweg
elevations. When USGS is unavailable, it automatically falls back to the
positive Sampled DEM Reach slope. Local negative slopes remain observable but
are never made positive, clamped, or replaced with another cross section's
slope.

If the requested or fallback source is not valid, only discharge is
unavailable: renderers return an explanatory table while map, cross-section,
and storage outputs continue. No zero, negative, or fabricated slope enters
the Manning calculation.

## Current architecture (practical view)

The app remains server-centric, with workflow behavior coordinated through `app_server` and helper functions.  
Instead of large structural rewrites, the architecture now emphasizes **small, explicit contracts** at fragile workflow boundaries.

Interactive flooding uses separate reactive timing lanes. Throttled REM values
drive Leaflet polygon replacement, while debounced values drive classification,
plots, volume, and discharge after slider motion settles. Results renderers are
registered once and consume the resulting reactive state; slider observers do
not recreate outputs. A bounded shared polygon cache avoids repeated
polygonization at revisited REM levels, and a geometry-generation-specific
volume lookup converts each settled elevation to an exact volume through
sorted thresholds and cumulative sums. Channel and floodplain classification
views are derived independently from immutable base cross-section points, so
one slider does not invalidate the other lane. Selecting another cross section
refreshes REM bounds from those cached points without terrain reprocessing.
Obsolete full water-surface raster state is not retained. See
`dev/features/interactive-flooding-responsiveness.md`.

For the Results path, this means:

- helper-driven state preparation for transition inputs and bounds,
- explicit readiness state produced by workflow helpers,
- a transition function that applies side effects (slider updates + readiness gate),
- a small injectable seam to control gate setting during tests.

## Key Results workflow design updates

### 1) Helper-backed transition state contract

Results transition logic depends on helper functions that return explicit workflow state, including:

- slider update values/bounds
- readiness flag (`results_loaded`)

This reduces hidden coupling and keeps transition behavior inspectable and testable outside of deep reactive internals.

### 2) Injectable gate-setter seam (architectural robustness change)

`run_results_workflow_transition()` now supports:

- `set_results_loaded = NULL` (default)

Behavior:

- When `set_results_loaded` is provided (function), transition readiness is propagated via the injected function.
- When omitted, existing internal behavior is preserved.

This seam is a deliberate design choice to make critical gate-setting behavior testable and deterministic while keeping runtime behavior unchanged by default.

See decision record: `dev/decisions/ADR-0003-results-workflow-gate-setter-seam.md`.

### 3) Transition boundary clarity

The Results transition boundary now cleanly separates:

- **state computation** (what readiness and slider state should be),
- **side effects** (updating inputs and setting readiness gate).

This improves maintainability and supports safer future extraction into modules/services.

## Design principles reinforced

### Explicit over implicit
Critical workflow state should be computed explicitly and propagated through clear contracts, not inferred from timing-sensitive reactive side effects.

### Narrow seams over broad rewrites
At fragile boundaries, introduce minimal seams that solve concrete reliability/testing problems without forcing a large refactor.

### Behavior-preserving evolution
Default production paths should remain stable while seams enable deterministic tests and confidence-building change.

### Test-guided hardening
If a workflow edge is difficult to test reliably, treat that as a design signal and add a small contract/seam rather than relying on brittle introspection.

## Known constraints

- The app is still monolithic in places; seams are incremental rather than complete modularization.
- Some reactive internals remain difficult to assert directly without introducing unnecessary coupling.
- Design changes should continue to prioritize low-risk, test-backed increments.

## Implications for future refactor work

The Results seam pattern should guide subsequent workflow hardening:

1. identify fragile transition boundaries,
2. extract/clarify state contracts,
3. isolate side effects,
4. add narrow injectable seams only where needed for deterministic testing,
5. back each change with focused regression tests.

Likely next targets:
- Draw XS transition orchestration
- Draw Flowline transition orchestration
- gradual movement from server-wide shared state toward clearer workflow-scoped contracts

## Relationship to testing strategy

This design aligns directly with
`dev/workflows/shiny-workflow-testing.md`:

- helper contract tests protect state computation
- transition integration tests protect workflow behavior
- server seam tests protect readiness propagation and session stability

The objective is not maximal abstraction; it is **robust behavior at high-risk boundaries with minimal change surface**.

## Status summary

The Results workflow design is now stronger than the prior baseline:

- transition state is explicit,
- readiness propagation is contract-based,
- gate-setting is testable via seam injection,
- repeat-run and fresh-session stability are regression-protected,
- production behavior remains intact under default path.

This is considered a meaningful architectural robustness improvement and is recorded in ADR-0003.
