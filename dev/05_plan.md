# 05 Plan

## Testing progress checkpoint

The initial Results workflow testing scaffold is in place and passing:
- server startup gating is covered
- slider state preparation is covered
- workflow readiness preparation is covered

The next step is to add the smallest possible server-level regression test for the Results transition, while keeping the suite behavior-oriented and avoiding brittle source/UI inspection.

## Current focus

Stabilize the Results workflow, preserve the working reactive fix, and build a testing framework that prevents the repeated reactive regressions that have been occurring in the app.

## Why testing is a priority now

The core engineering calculations are already covered in `fluvgeo`. The remaining risk is not the math; it is the Shiny reactive workflow and the orchestration around it.

The goal of this test effort is to lock in the reactive best practices identified during troubleshooting so that the same class of mistakes does not recur.

## Immediate accomplished work to preserve

- Resolved the silent reactive failure in `view_results`.
- Confirmed the root cause was reactive timing around programmatic slider updates.
- Fixed the issue by capturing slider values into local variables before calling `updateSliderInput()`.
- Restored reliable automatic navigation to the Results tab.
- Confirmed that plots and discharge tables now render successfully when the Results workflow completes.

## Testing reference

Detailed testing strategy, reactive workflow conventions, and regression cases are documented in `dev/20_testing.md`. The plan tracks the implementation order and near-term test tasks; the testing doc defines the durable testing rules and priorities.

## Testing goals

The test framework should enforce the following behaviors:

1. The Results workflow completes reliably.
2. Programmatic input updates do not read and write the same reactive state in an unsafe way.
3. First-run initialization succeeds.
4. Repeated workflow execution does not break the app.
5. Results outputs are only available after the workflow is properly initialized.
6. The reactive patterns validated during troubleshooting remain the required behavior going forward.

## Proposed testing layers

### 1. Workflow regression tests
Use these to protect the user-facing Results path.

Minimum cases:
- triggering `view_results` completes without error
- the app reaches a results-ready state
- the app navigates to the Results tab
- expected results outputs become available

### 2. Reactive contract tests
Use these to enforce the best-practice patterns that prevented the recent bug.

Minimum cases:
- slider updates use captured local values, not unstable live reactive reads
- no observer both consumes and mutates the same reactive input in the same step
- `results_loaded` or equivalent gating behaves consistently
- outputs are not rendered before initialization is complete

### 3. Smoke tests
Use these as lightweight checks for catastrophic regressions.

Minimum cases:
- app launches successfully
- Results workflow can be reached
- a basic happy-path run still works after changes

## Priority test cases

### P1: Results workflow happy path
- simulate the inputs required to run Results
- trigger `view_results`
- verify the workflow completes
- verify results state is set
- verify key outputs are available

### P1: Slider update safety
- verify programmatic slider updates do not fail during Results initialization
- verify slider values remain valid after updates
- verify the workflow does not silently stall during slider mutation

### P1: First-run stability
- verify the first Results run succeeds
- verify initialization does not depend on a prior hidden state
- verify no silent reactive failure occurs on initial load

### P2: Repeat-run stability
- run the Results workflow a second time
- verify outputs still render
- verify state is not corrupted by the prior run

### P2: Output availability gating
- verify outputs are not available before results are ready
- verify outputs appear after initialization completes

## Preferred implementation approach

- Start with `testthat`-based tests for workflow behavior.
- Use Shiny workflow testing tools such as `testServer()` for reactive server logic.
- Extract helper functions only where needed to make the workflow testable.
- Keep the tests focused on the fragile reactive boundaries rather than trying to test every UI detail.

## Design principles to enforce through tests

The tests should codify the following rules:

- capture reactive values before using them in `update*Input()` calls
- avoid coupled read/write behavior in the same observer whenever possible
- prefer explicit workflow state to implicit sequencing
- keep output definitions stable
- make initialization boundaries obvious and testable

## Near-term work items

1. Identify the Results workflow entrypoint and its observable state transitions.
2. Write the first regression test for the happy-path Results run.
3. Write the first test for safe slider update behavior.
4. Add a first-run stability test.
5. Add a repeat-run stability test.
6. Expand coverage only after the highest-risk regressions are protected.

## Definition of done for this testing phase

This phase is complete when:
- the recent Results regression is covered by tests,
- the repeated reactive mistakes are prevented by tests,
- and the app has a small but meaningful Shiny workflow test suite guarding the most fragile behavior.
