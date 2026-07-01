# ADR-0003: Add injectable gate-setter seam to Results workflow transition

- **Status:** Accepted
- **Date:** 2026-07-01
- **Deciders:** Maintainers of `FluvialGeomorph/ohwm2`
- **Supersedes:** None
- **Superseded by:** None

## Context

The Results workflow has historically been a fragile reactive boundary in the Shiny server layer.  
Recent regression testing and debugging identified repeated risks in this area:

- silent transition failures
- first-run instability
- repeat-run/session instability
- brittle coupling to implicit reactive wiring when attempting server-level tests

`run_results_workflow_transition()` previously depended on direct internal readiness-gate invocation (`results_loaded(...)`) in a way that was not reliably testable in isolated server test contexts. This made it difficult to assert gate-setting behavior without depending on hidden reactive internals or brittle test assumptions.

## Decision

Add an **injectable gate-setter seam** to the Results transition orchestration function:

- `run_results_workflow_transition(..., set_results_loaded = NULL)`

Behavior:

1. The transition computes `workflow_state` using existing workflow helper logic.
2. If `set_results_loaded` is provided and is a function, call it with `workflow_state$results_loaded`.
3. Otherwise, preserve existing default behavior using the current internal gate setter path.

This introduces a small architectural seam at a critical workflow boundary while preserving production behavior by default.

## Rationale

This decision is intended to increase robustness and testability with minimal structural churn:

- Creates an explicit contract for readiness propagation at the transition boundary.
- Enables deterministic server-level tests for gate-setting behavior.
- Reduces reliance on implicit reactive scope assumptions in tests.
- Preserves existing runtime behavior when no injection is supplied.
- Supports incremental modularization by clarifying orchestration responsibilities.

The seam is deliberately narrow and targeted to avoid over-refactoring while addressing a repeated source of instability.

## Consequences

### Positive

- Server-level transition tests can assert readiness propagation directly.
- Repeat-run and fresh-session stability tests become more reliable.
- Reduced risk of regressions in Results gating behavior.
- Better separation between transition computation and reactive side effects.
- Improves maintainability and confidence for future workflow refactors.

### Negative / Costs

- Slightly expanded function signature.
- Additional responsibility to keep seam semantics stable over time.
- Potential misuse if callers pass inconsistent setter behavior.

### Mitigations

- Default behavior remains unchanged when `set_results_loaded` is `NULL`.
- Tests enforce expected seam behavior (setter called with readiness value).
- Keep seam internal and documented as a transition contract, not a public UI API.

## Alternatives considered

1. **Do nothing / test only helper outputs**
   - Rejected because it does not validate server transition gate propagation.

2. **Test hidden reactive internals directly**
   - Rejected because brittle and environment-dependent.

3. **Larger immediate modular refactor**
   - Rejected for now; higher change surface than needed to secure this boundary.

## Implementation notes

- Added roxygen documentation to `run_results_workflow_transition()` for `set_results_loaded`.
- Updated server tests to inject capture setters where needed.
- Added/updated tests for:
  - first Results run readiness
  - repeat-run stability
  - fresh-session repeat stability
  - readiness/output gating behavior
- Full test suite passes after seam adoption.

## Verification

Decision considered verified when:

- transition seam tests pass consistently
- repeat-run/fresh-session Results stability tests pass
- no behavior regressions in normal app execution
- default path (no injected setter) continues to work as before

Current status: **verified by passing test suite and completed transition regression coverage**.

## Follow-up

- Continue incremental workflow modularization using similarly narrow, test-driven seams.
- Extend equivalent transition robustness patterns to Draw XS and Draw Flowline workflows where warranted.
