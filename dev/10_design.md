## Server architecture — current state and known limitations
_Added: 2026-05-31_

The current `app_server.R` is a second-generation imperative design with the
following known structural limitations that are scheduled for refactor (Option 4):

- All reactive state is managed via `<<-` global assignment rather than
  `reactiveVal` / `reactiveValues`
- The `view_results` observer is a large imperative block rather than an
  `eventReactive` returning a results list
- Slider observers (`channel_elevation`, `floodplain_elevation`) are top-level
  but guard on a `results_loaded` flag rather than being scoped to post-results state
- A `freezeReactiveValue` + `isolate` patch (applied 2026-05-31) addresses the
  immediate first-run hang; full architectural fix is deferred to the Option 4 refactor

The intended target architecture (post-refactor) is:
- `eventReactive(input$view_results)` returns a named list of all computed objects
- All reactive state held in `reactiveVal` / `reactiveValues`
- Slider observers are top-level with `ignoreInit = TRUE`; no `results_loaded` flag needed
- All expensive computation steps guarded with `validate()`/`need()`
