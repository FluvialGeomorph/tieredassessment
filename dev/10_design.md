# 10 Design

## Current application design

The application is a Shiny-based geospatial workflow app for drawing cross sections and flowlines, computing terrain-derived results, and presenting those results on a dedicated Results tab.

The current server implementation still works, but it has reached a complexity level where the reactive flow is no longer easy to reason about mentally. The app currently depends on a mixture of:
- large imperative observers
- programmatic input updates
- shared reactive state
- navigation side effects
- output registration during workflow execution

This design is functional, but it is not yet the maintainable target architecture.

## Current state of the Results workflow

The Results workflow currently follows this pattern:

1. The flowline geometry is collected.
2. DEM-based derivatives are computed.
3. Cross-section and water-surface outputs are derived.
4. Slider ranges are updated programmatically.
5. Results state is marked ready.
6. The app navigates to the Results tab.
7. Outputs render using the stabilized state.

A key implementation lesson from this workflow is:

- When updating a reactive input inside the same observer that reads it, capture the current value into a local variable first.
- Do not rely on direct `isolate(input$...)` inside `updateSliderInput()` when the observer is still in a reactive transition.

That pattern prevented a silent Shiny failure in the Results workflow.

## Known limitations

The current structure has the following limitations:

- `app_server.R` is still too large and stateful to be easy to hold in memory.
- Several concerns are still coupled together:
  - computation
  - navigation
  - reactive state updates
  - output rendering
  - initialization
- The current workflow depends on implicit ordering that is fragile under Shiny reactivity.
- The server file contains more orchestration than is comfortable for long-term maintenance.

## Architectural direction

The intended direction is a phased refactor toward a modular architecture that makes the reactive flow more explicit and easier to maintain.

Future-state goals:

- Use Shiny modules for major workflow boundaries.
- Separate orchestration from computation and rendering.
- Reduce hidden reactive coupling.
- Keep stable renderers defined outside large event observers where possible.
- Make initialization state explicit rather than implicit.
- Minimize the need for programmatic updates to inputs that are also consumed in the same reactive path.

## Preferred design principles

### Prefer
- small observers with one responsibility
- explicit workflow state
- module boundaries for major features
- stable top-level render functions
- local variables for transient values used in input updates
- clear separation between setup and rendering

### Avoid
- monolithic observers that perform many unrelated actions
- reading and writing the same reactive input in the same control path
- hidden sequencing assumptions
- using reactive workarounds as permanent architecture
- overloading one server file with all workflow logic

## Design truth to carry forward

The application is valuable and functional, but the current architecture is beyond what is reasonable to maintain mentally as a monolithic reactive implementation.

The next phase of development should preserve current behavior while deliberately moving the app toward a modular, explicit, and more human-maintainable design.
