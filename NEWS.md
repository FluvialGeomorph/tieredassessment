# ohwm2 2026.07.25.9000

* Results observers now normalize Manning's values from Shiny select controls
  before validation, restoring channel and floodplain REM recalculation when
  their sliders change.
* Results slider initialization now extracts detrended elevations as an
  attribute vector from production `sf` cross-section points, preventing an
  `is.finite()` list-method failure when **View Results** is selected.
* Draw XS location search now normalizes the Leaflet Search formatter contract,
  preventing OpenStreetMap results from rendering as `undefined`.
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
