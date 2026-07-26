# ohwm2 2026.07.25.9000

* Results now provides a slope-scale control with **USGS Reach** as the default
  and **Local DEM** as an explicit exploratory alternative. Lookups use bounded
  retry/backoff and a request timeout, slopes are cached by cross section, and
  slider/Manning recalculation no longer repeats the remote request.
* When USGS is unreachable or coverage is missing, discharge continues with
  the selected cross section's signed Local DEM slope where it is positive.
  Negative local slopes are reported but never transformed or substituted.
  Persistent status
  messaging and a manual retry action explain the degraded mode; map,
  cross-section, and storage results remain available even if no slope can be
  resolved.
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
