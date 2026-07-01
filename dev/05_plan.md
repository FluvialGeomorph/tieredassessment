# 05 Plan

## Testing progress checkpoint

**Complete slider observer refactoring and helper integration (21 new tests):**

**Results workflow integration tests (6 tests):**
- workflow completes without error
- slider bounds computed correctly for selected cross-section
- slider values captured and preserved
- slider values remain valid within bounds (safety check for programmatic updates)
- cross-section selection works across different cross-sections
- results_loaded flag set correctly after state preparation

**Slider elevation update helper tests (7 tests):**
- channel elevation update captures slider values correctly
- floodplain elevation update captures slider values correctly
- elevation value validation works for valid values
- elevation value validation rejects out-of-bounds values
- elevation validation rejects invalid cross-section
- channel elevation update rejects missing required data
- floodplain elevation update rejects missing required data

**Slider Manning's n update helper tests (8 tests):**
- channel Manning's n update captures slider values correctly
- floodplain Manning's n update captures slider values correctly
- Manning's n value validation works for valid values
- Manning's n value validation rejects out-of-range values
- Manning's n value validation rejects non-numeric values
- channel Manning's n update rejects missing required data
- floodplain Manning's n update rejects missing required data

**Observers refactored to use helpers:**
- All four slider observers (`input$channel_elevation`, `input$floodplain_elevation`, `input$channel_mannings`, `input$floodplain_mannings`) now call validation helpers at the start
- Observers gate helper calls with `req()` checks to prevent premature execution
- Tests act as regression guard rail for future reactive changes
- Reactive safety patterns (captured values, explicit state) are now enforced structurally

---

## New progress update (post-checkpoint)

**Results workflow reliability expanded with additional transition + session-stability coverage:**

- Added repeat-run regression coverage for Results workflow state preparation:
  - repeated Results workflow execution preserves readiness and slider-state invariants
  - repeat execution remains stable without hidden priming assumptions

- Added output/readiness gating coverage:
  - explicit test verifies readiness gate behavior before/after workflow preparation
  - confirms Results availability is tied to workflow-ready state

- Introduced and validated a small server transition seam for testability:
  - `run_results_workflow_transition()` now supports injectable `set_results_loaded`
  - enables deterministic server-level testing of gate-setting behavior without brittle reactive coupling
  - avoids prior missing-argument failures tied to implicit gate setter wiring

- Added server-level transition stability tests across fresh sessions:
  - first Results run reaches ready state
  - repeated runs in fresh server sessions stay stable
  - injected gate setter receives expected ready-state signal (`TRUE`)

- All tests currently passing after seam update and test adjustments.

## Current focus

Stability is achieved and reinforced. The Results workflow is now protected at multiple layers:
- helper contracts,
- workflow transition integration,
- server transition seam behavior across fresh sessions.

This expands protection for the highest-risk regression classes (silent transition failure, first-run instability, repeat-run/session instability) while preserving a small, maintainable test surface.

## Why testing remains the priority

The core engineering calculations are already covered in `fluvgeo`. The remaining risk is Shiny orchestration and reactive sequencing.  
Recent work further converts fragile reactive behavior into explicit, testable contracts and keeps changes reviewable.

## Immediate accomplished work to preserve

- Resolved the silent reactive failure in `view_results`.
- Confirmed reactive timing around programmatic slider updates as root cause.
- Fixed workflow reliability by captured-value update pattern before `updateSliderInput()`.
- Restored automatic navigation and successful Results rendering.
- Added and maintained regression tests around Results transition and slider safety behavior.
- Added repeat-run/session-stability tests and injectable gate-setter seam for server transition testing.
- Confirmed full test suite passes after seam and test updates.

## Testing reference

Detailed testing strategy and regression classes remain in `dev/20_testing.md`.

The Results transition now has:
- helper-backed state preparation tests,
- integration-level transition tests,
- server-level seam tests validating gate-setting behavior in controlled sessions.

## Test suite status (updated)

- **Total tests written:** expanded beyond initial 21 (all currently passing)
- **Helper functions created:** 6+ (including workflow transition seam support)
- **Observers refactored:** 4 (all working with validation guard rails)
- **Regression classes protected:** silent reactive failures, unsafe input coupling, first-run instability, repeat-run/session instability, slider bounds violations, readiness gating behavior

## Definition of done for this testing phase

✅ This phase is complete and reinforced:
- recent Results regressions are covered by tests
- repeated reactive mistakes are prevented by tests
- repeat-run/session stability now has explicit regression protection
- readiness gating behavior is covered
- server transition behavior is testable through an injectable seam
- future reactive changes can be made with stronger confidence

## Next steps

With this reliability layer in place, proceed in small increments:

1. **Test hygiene pass**
   - remove duplicate/overlapping test cases
   - keep clear boundaries between helper, integration, and server-seam tests

2. **Additional workflow coverage**
   - extend equivalent transition/regression tests to Draw XS and Draw Flowline workflows

3. **Incremental modular refactor support**
   - continue extracting orchestration seams from `app_server.R`
   - keep each seam covered by behavior-oriented tests before broader modularization

## Next execution slice: Draw XS workflow hardening (seam-first)

Apply the same robustness pattern used for Results to the Draw XS transition boundary, in small test-backed increments.

### Scope (this slice only)
- Focus exclusively on Draw XS transition behavior.
- Do not begin broad module decomposition in this slice.
- Do not combine Draw XS and Draw Flowline in one change set.

### Planned sequence

1. **Transition boundary identification**
   - confirm Draw XS entrypoint/observer path in `app_server.R`
   - identify readiness/output gate(s) and transition side effects

2. **Helper contract preparation**
   - add or confirm a helper that returns explicit Draw XS transition state
   - include any computed bounds/selected state needed for deterministic assertions

3. **Injectable seam addition**
   - add a narrow optional gate-setter argument to Draw XS transition function
   - preserve current behavior when seam is not provided (default path unchanged)

4. **Tests before broader changes**
   - add helper contract tests for Draw XS state preparation
   - add transition integration tests
   - add server seam test asserting gate setter is called with expected readiness value

5. **Stability follow-through**
   - add first-run stability test
   - add repeat-run stability test (fresh sessions where applicable)

6. **Minimal app wiring update**
   - update `app_server.R` call site(s) to new seam argument name
   - avoid unrelated refactors in the same commit series

### Exit criteria for this slice
- Draw XS first-run transition passes reliably
- repeat/fresh-session behavior is stable
- gating behavior is test-covered
- all tests green
- docs updated with completed coverage and any new seam decision
