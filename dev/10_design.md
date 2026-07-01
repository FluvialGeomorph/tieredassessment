# 10 Design

## Overview

This app’s core engineering computations are provided by `fluvgeo`.  
The primary design challenge in this repository is Shiny orchestration: reactive workflow transitions, initialization order, and output readiness gating.

Recent reliability work focused on the Results workflow boundary, where reactive sequencing had produced silent failures and inconsistent run behavior.

The current design direction is:

1. preserve working behavior,
2. improve robustness at critical transition boundaries,
3. expose narrow orchestration seams for deterministic testing,
4. continue incremental modularization with regression protection in place.

## Current architecture (practical view)

The app remains server-centric, with workflow behavior coordinated through `app_server` and helper functions.  
Instead of large structural rewrites, the architecture now emphasizes **small, explicit contracts** at fragile workflow boundaries.

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

This design aligns directly with `dev/20_testing.md`:

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
