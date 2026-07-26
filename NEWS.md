# ohwm2 2026.07.25.9000

* Results initialization now clamps channel and floodplain slider values to
  the selected cross section's computed range before updating the Shiny
  inputs, preventing invalid-value warnings that blocked Results review.
* Added a validated application skin layer for task-specific identity, theme,
  navigation labels, guidance, progress messages, and favicon assets while
  preserving shared workflow behavior.
* Added `validate_app_skin_file()` for downstream and CI skin preflight checks.
* Results processing no longer depends on remote watershed delineation.
  DEM-derived geometry and discharge results continue with drainage area
  omitted when that enrichment is not requested.

# ohwm2 2025.07.08

* Initial creation.
