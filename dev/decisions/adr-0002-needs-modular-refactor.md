# ADR 0002: Refactor the Shiny app into a modular, maintainable architecture

Date: 2026-05-31

## Status
Proposed

## Context

This application began as an exploratory proof of concept and grew incrementally as features were added and troubleshooting unfolded. That process successfully demonstrated the app’s value, but it also produced a server implementation that is now too reactive, too stateful, and too intertwined for a human developer to reason about reliably.

In particular, the current `app_server.R` has accumulated:
- large observers that combine computation, navigation, input mutation, output creation, and error handling
- hidden reactive dependencies that are difficult to trace mentally
- programmatic input updates that can trigger silent Shiny failures if the reactive state is not carefully managed
- output definitions that are created inside event handlers rather than established as stable renderers
- too many responsibilities in a single file

Today’s debugging showed that the app can enter failure states that are extremely difficult to reason about without extended instrumentation and AI-assisted tracing. This is a strong signal that the current structure has outgrown what is practical to maintain as a monolithic Shiny server file.

## Problem

The current app structure makes it too easy to introduce reactive timing bugs, silent errors, and tangled dependencies.

Examples of the underlying issues include:
- reading and writing the same reactive inputs in the same observer
- updating sliders programmatically while also depending on their values in downstream render logic
- defining renderers inside large event observers
- coupling navigation, computation, and UI initialization into one workflow
- relying on implicit reactive ordering instead of explicit state boundaries

This creates a maintenance burden that is disproportionate to the app’s complexity. It also makes it difficult for a developer to confidently extend the app without re-triggering subtle failures.

## Decision

We will treat the current server implementation as technical debt and refactor the app toward a modular architecture.

Future development should:
- separate workflow orchestration from computation and rendering
- use Shiny modules for major feature areas
- keep outputs defined in stable, top-level renderers where possible
- minimize cross-talk between input mutation and input consumption
- make first-load and initialization state explicit rather than implicit
- reduce the amount of shared mutable state inside a single observer

## Consequences

### Positive
- The app becomes easier to understand and extend.
- Reactive behavior becomes more predictable.
- Bugs caused by hidden sequencing become easier to isolate.
- Feature work can be decomposed into clearer boundaries.
- Human maintainability improves significantly.

### Negative
- The refactor will take time.
- Some current patterns will need to be rewritten rather than patched.
- There may be short-term duplication while logic is extracted into modules.

## Lessons from the current debugging session

One concrete lesson from today:

> When programmatically updating a reactive input inside an observer that also depends on that input, capture the value into a local variable first. Do not call `isolate(input$...)` directly inside the `update*Input()` call unless the reactive state is known to be stable.

This pattern caused a silent Shiny error during `view_results` and was resolved by using local variables before calling `updateSliderInput()`.

## Architectural guidance going forward

### Prefer
- feature-specific Shiny modules
- explicit reactive state objects for workflow stages
- stable renderer definitions outside large event handlers
- small observers with one responsibility
- clear naming for initialization versus live reactive values

### Avoid
- defining outputs inside large observers unless absolutely necessary
- mixing navigation and computation in the same control path when it can be avoided
- mutating reactive inputs and consuming them in the same step
- large monolithic server functions that require full mental simulation to debug

## Recommended next steps

1. Preserve the current known-good behavioral fix.
2. Plan a phased refactor of `app_server.R` into smaller units.
3. Identify feature boundaries suitable for modules, especially:
   - Draw XS workflow
   - Draw Flowline workflow
   - Results workflow
   - plots/tables/rendering
4. Introduce explicit initialization state where needed.
5. Use this ADR as the architectural guardrail for future changes.

## Notes

This ADR is not a critique of the app’s origin. It records the reality that the application evolved successfully as a proof of concept, but now requires deliberate architectural investment to remain maintainable and extensible.