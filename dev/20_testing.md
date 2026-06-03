# 20 Testing

## Current progress

The initial Results workflow helper-based regression tests are in place and passing; the next step is to add the smallest possible server-level transition test without relying on brittle source/UI inspection.

## Purpose

This document defines the testing strategy for the app’s Shiny workflow behavior, with special focus on preventing the reactive regressions that have caused repeated failures in the Results workflow.

The engineering calculations themselves are already tested in `fluvgeo`. The primary testing gap in this repository is the Shiny reactive orchestration layer: workflow transitions, initialization, programmatic input updates, output availability, and state gating.

This document is intended to be a durable guide for:
- designing tests
- prioritizing test coverage
- encoding safe reactive patterns
- preventing regression of known failure modes
- supporting future refactoring work

## Testing goals

The test suite should ensure that:

1. The Results workflow completes reliably.
2. First-run initialization succeeds.
3. Programmatic input updates do not create unstable reactive transitions.
4. The app does not silently stall during workflow progression.
5. Results outputs appear only after required state is ready.
6. Repeated workflow execution does not corrupt state.
7. Safe reactive patterns validated during troubleshooting remain enforced over time.

## Non-goals

This testing framework is not intended to:
- re-test the engineering calculations already covered in `fluvgeo`
- replace architectural refactoring
- make the app’s monolithic structure “safe enough” to keep forever
- test every visual detail of the UI exhaustively

The purpose is to protect the workflow boundaries that have been historically fragile.

## Testing philosophy

The testing strategy should be layered.

### 1. Workflow regression tests
These protect the user-facing Results path.

They should verify things like:
- triggering `view_results` completes successfully
- the app reaches a results-ready state
- results outputs become available
- navigation to the Results tab occurs
- no silent reactive failure happens during the workflow transition

### 2. Reactive contract tests
These protect the safe Shiny patterns discovered during troubleshooting.

They should verify things like:
- programmatic slider updates use stable captured values
- observer logic does not read and write the same reactive input unsafely in one path
- initialization is explicit and stable
- outputs are gated until the workflow is ready

### 3. Smoke tests
These provide lightweight confidence that the app still behaves at a basic level.

They should verify things like:
- the app launches
- the Results workflow can be reached
- a normal happy path still works after changes

## Priority regression classes

The following failure modes should be protected by tests:

### 1. Silent failure during Results initialization
This is the most important regression class.

The test suite should catch cases where:
- the Results workflow stalls
- a reactive update fails silently
- outputs never appear
- the app appears to hang at a workflow transition

### 2. Unsafe programmatic input updates
The suite should catch patterns where:
- an observer updates a slider while relying on a live unstable reactive read
- the same observer both consumes and mutates the same input in a fragile way
- initialization logic depends on reactive timing rather than explicit state

### 3. First-run instability
The suite should catch cases where:
- the first attempt to reach Results fails
- hidden initialization order matters too much
- the workflow only succeeds after an earlier run primes state

### 4. Repeat-run corruption
The suite should catch cases where:
- running Results a second time changes behavior unexpectedly
- state from a previous run leaks into the current one
- outputs or slider values become invalid after repeated use

## Reactive best practices to enforce

These tests should encode the following rules as durable expectations:

- Capture reactive values into local variables before using them in `update*Input()` calls.
- Avoid reading and writing the same reactive input in the same control path when possible.
- Prefer explicit workflow state over implicit sequencing.
- Keep output registration stable where possible.
- Make initialization boundaries observable and testable.
- Treat silent failures as test failures, not as “just a Shiny thing.”

## Recommended test layers in this repository

The Results transition is now tested through a small helper-backed server seam that normalizes the workflow state before marking `results_loaded`. This keeps the workflow test behavior-oriented while avoiding brittle UI/source inspection.

### Layer A: server workflow tests
Use these to validate the Results workflow and other major reactive transitions.

Likely tool:
- `shiny::testServer()`

Good candidates:
- Results entrypoint behavior
- slider update behavior
- `results_loaded` or equivalent gating
- output readiness after workflow completion

### Layer B: helper / orchestration tests
Where workflow logic can be isolated into helper functions or small orchestration units, test those separately.

This is especially useful for:
- computing update parameters
- validating input state before a reactive transition
- preparing data structures for output rendering

### Layer C: app smoke tests
Use these sparingly to verify the app still starts and can complete a basic happy path.

Good candidates:
- launch without error
- reach Results tab
- basic post-click state availability

## Initial test cases to implement

The first test suite should likely include the following cases:

### P1: Results happy path
Verify that:
- required inputs are present
- `view_results` is triggered
- the workflow completes
- results state is set
- outputs become available

### P1: Slider update safety
Verify that:
- slider updates happen without silent failure
- slider bounds and values remain valid
- workflow completion is not interrupted by input mutation

### P1: First-run stability
Verify that:
- the first run of Results succeeds
- no hidden prior state is required
- initialization completes consistently

### P2: Repeat-run stability
Verify that:
- a second run behaves the same as the first
- state does not leak across runs
- outputs remain valid

### P2: Output gating
Verify that:
- outputs are not available before the workflow is ready
- outputs are available after initialization completes

## Testing workflow guidance

When adding tests:

1. Start with the fragile workflow transition.
2. Encode the bug that was just fixed as a regression test.
3. Add only the next most likely failure mode.
4. Keep tests focused on behavior, not implementation details.
5. Prefer a few high-value tests over many brittle ones.

## How this fits the refactor

A good test suite should make refactoring safer by providing a behavioral safety net.

As the app is modularized:
- tests should move closer to the units they protect
- workflow tests should remain in place for end-to-end coverage
- regression tests for the current failure modes should remain permanently
- additional module-level tests can be added as the architecture improves

## Maintenance expectations

This document should be updated when:
- a new class of reactive regression is discovered
- a testing convention changes
- the app architecture changes enough to alter test structure
- a new workflow boundary becomes important to protect

## Definition of done for the testing framework

The testing framework is mature enough for this phase when:

- the Results regression is covered by tests
- safe reactive update patterns are enforced
- first-run and repeat-run stability are covered
- the app has a small but meaningful suite of Shiny workflow tests
- future reactive changes can be made with much lower risk of repeating the same bugs
