# ohwm2 2026.07.27.1

* Pinned `aws.signature 0.6.0` to its immutable upstream source commit so
  package installation does not attempt to read Connect Cloud's unavailable
  build-time OIDC token file while loading `mapboxapi`.
* Pinned `terra 1.9-27` to its immutable upstream source commit. This is the
  newest release before `terra` began calling a GDAL 3.8 API that is absent
  from Posit Connect Cloud's GDAL 3.4.1 build environment. Deployment metadata
  now targets Connect Cloud's documented R 4.6.0 runtime.

# ohwm2 2026.07.27
* Pinned `terra 1.9-34` to its immutable upstream source commit so managed
  Linux deployments compile against the geospatial libraries in their runtime
  image instead of reusing an ABI-incompatible hosted binary.

# ohwm2 2026.07.26.1
* Documented the one-time migration of existing customer repositories and the
  GitHub-backed Posit Connect Cloud deployment contract, including stable
  production URLs, explicit operator approval gates, and recovery boundaries.
* Deployment metadata no longer treats the package-style Shiny application as
  its own installed dependency. The manifest now includes the complete shared
  release source and excludes the obsolete `shinyValidator` record.

# ohwm2 2026.07.26.9000
* Interactive flooding map polygons now follow REM slider motion through a
  throttled fast path, while plots, storage, and discharge recalculate after
  the slider settles. Results outputs are registered once, and polygon updates
  preserve the current map viewport.
* Interactive flooding now reuses a bounded cache of water-surface polygons,
  calculates exact storage volumes from a precomputed lookup, and updates only
  the Channel or Floodplain classification changed by each slider. Base
  cross-section points remain immutable, selected cross sections refresh REM
  bounds without terrain reprocessing, and unused water-surface raster state
  is no longer retained.
* Reach-slope lookup now returns promptly outside USGS NHDPlus coverage,
  including locations with otherwise valid DEM coverage, and immediately
  continues with a valid Sampled DEM Reach slope instead of retrying an
  inapplicable raindrop trace.
* Results now exposes three explicit slope scales: **USGS Reach**, **Sampled
  DEM Reach**, and **Local XS Neighborhood**. The sampled reach value uses the
  minimum and maximum elevations and profile length of the flowline points
  plotted in the longitudinal profile.
* USGS Reach and Sampled DEM Reach are cached once for every cross section.
  The entire Local XS Neighborhood profile is also calculated and cached in
  one pass, making scale and cross-section changes immediately responsive.
* Backward workflow navigation now uses explicit raw-geometry snapshots.
  Every **Draw Flowline** action resubmits the current cross-section editor
  contents, retrieves a matching DEM, and creates a fresh Flowline editor;
  every **View Results** action resubmits the current flowline and recomputes
  Results from the raw cross sections rather than previously processed output.
* DEM requests now receive a local small-site extent preflight before the
  terrain service is contacted. The default buffered span limit is 10 km
  (`options(ohwm2.max_dem_span_m = ...)` can adjust it), and out-of-map,
  no-coverage, empty-raster, and unavailable-service outcomes produce
  recoverable user messages.
* Repeated **View Results** runs now rebuild the cross-section selector from
  the latest drawn geometry, so added and deleted cross sections are reflected
  immediately. Cross sections without a usable sampled terrain range are
  omitted with a warning instead of blocking all Results.
* Results now displays a distinct progress stage while local and USGS reach
  slope data are prepared, eliminating the unexplained pause after geometry
  rendering.
* The deferred post-flush slope lookup now reads the Results readiness gate in
  an isolated Shiny context, preventing **View Results** from raising
  `Operation not allowed without an active reactive context`.
* Results now provides a slope-scale control with **USGS Reach** as the default.
  Lookups use bounded retry/backoff and a request timeout, and slider/Manning
  recalculation no longer repeats the remote request.
* When USGS is unreachable or coverage is missing, discharge continues with
  the Sampled DEM Reach slope where it is positive. Negative local
  neighborhood slopes are reported but never transformed or substituted.
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
