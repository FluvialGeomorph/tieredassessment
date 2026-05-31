# 2026-05-31 — Results tab reactive debugging summary

## Context
We spent this session debugging the `ohwm2` app’s `View Results` flow in `R/app_server.R`. The original symptom was a first-run hang on the Results page, followed by a series of reactive timing and initialization issues discovered during troubleshooting.

## What we learned

### 1) The geometry calculations were not the primary failure
Console logs showed that the `observeEvent(input$view_results, ...)` logic was completing the expensive geometry and table calculations successfully:
- flowline processing completed
- REM / floodplain volume calculations completed
- results map and plots were being assigned
- slider range values were being computed

This means the original issue was not “slow computation” or an outright failure in the geometry pipeline.

### 2) The first major bug was reactive sequencing around slider updates
The app was updating `channel_elevation` and `floodplain_elevation` programmatically during `view_results`.
That was triggering downstream observers too early, before the results state was fully ready.

The initial mitigation path involved:
- `freezeReactiveValue()` before `updateSliderInput()`
- `isolate()` on slider values during one-time calculations
- moving spinner cleanup later in the flow

### 3) Spinner cleanup needed to be guaranteed
The modal spinner could remain stuck if any late-stage error occurred before cleanup.

A guaranteed cleanup path was added with:
- `on.exit(remove_modal_spinner(), add = TRUE)`
- later, `tryCatch(..., finally)`-style reasoning around the observer

### 4) `nav_select()` order mattered
We learned that the tab switch to Results depended on when navigation occurred relative to the rest of the observer.

After experimentation, the app began switching to Results again once the navigation timing was adjusted.

### 5) The remaining bug was not navigation; it was first-render plot state
Once navigation worked, the cross-section plots still did not show the blue bankfull line on first load.
Manual slider movement caused the plots to update correctly.

That showed:
- the slider widgets were functional
- the plot logic was functional
- but the initial reactive state for the plots was not propagating cleanly from the programmatic slider setup

### 6) `sliderInput` vs `noUiSliderInput` was probably a red herring for this symptom
The widget choice may matter in general, but the evidence from this session suggests the current issue is not primarily widget implementation.
The important problem is that programmatic slider initialization does not behave like a user-driven slider event for downstream reactive invalidation.

### 7) A server-side initialization state is likely needed
The most promising structural fix is to separate:
- live slider input values, from
- one-time initial values used to render the Results tab cleanly

This led to the idea of using `reactiveVal()` initialization state in `app_server.R`, such as:
- `channel_elevation_init`
- `floodplain_elevation_init`

Those should be set once during `view_results` and used to drive first-load rendering.

## Practical conclusions
- The problem evolved from a spinner hang into a first-render reactive initialization issue.
- The reactive complexity of `app_server.R` made the debugging path hard to reason about in-place.
- The app would likely benefit from either:
  1. a rollback to a cleaner known-good commit, then minimal reapplication of only the proven fixes, or
  2. a deliberate refactor that separates geometry calculation, UI navigation, and first-load initialization.

## Recommended future approach
1. Revert to the last known-good baseline before the troubleshooting branch explosion.
2. Reapply only the minimal fixes now proven necessary:
   - guaranteed spinner cleanup
   - correct navigation timing
   - server-side initialization state
3. Keep live slider observers separate from first-load Results initialization.
4. Avoid adding more temporary duplication inside `view_results`; it makes the reactive flow much harder to debug.

## Bottom line
This session established that the app’s remaining problem is not the heavy computation itself.
It is the mismatch between:
- programmatic UI initialization, and
- the reactive state needed for first-render Results tab plots.