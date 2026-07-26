# Results without watershed-service dependency

## User-visible behavior

After users draw cross sections and a flowline, the Results transition does not
block on remote watershed delineation or USGS stream-network enrichment. The
workflow continues through REM creation, cross-section stationing,
classification, plots, and volume calculation before the reach-slope lookup
runs.

USGS NHDPlus reach slope is preferred for the Manning calculation. The app:

- defaults the **Slope scale** control to **USGS Reach (recommended)** while
  allowing the user to select **Local DEM** for local-scale exploration,
- bounds remote requests with a timeout and retry/backoff policy,
- caches the result per cross section so slider and Manning changes do not
  repeat a live request,
- retries the currently selected cross section when the user requests it,
- falls back to the selected cross section's Local DEM slope when USGS is
  unreachable or returns no usable coverage and that signed slope is positive,
  and
- leaves map, cross-section, and storage results usable if neither source
  yields a scientifically valid positive slope.

The Discharge panel persistently identifies the requested scale, applied source,
and applied slope value, including whether it is checking USGS, using cached
USGS data, using the DEM fallback, or unable to calculate discharge.
Degraded states also produce a notification and expose a **Retry USGS slope**
action. Raw service errors remain in application diagnostics rather than user
messages.

## Data behavior

- `Watershed_Area_SqMile` remains present and numeric.
- Its value is missing because the OHWM workflow explicitly skips watershed
  enrichment.
- DEM-derived area, width, and depth are calculated through
  `fluvgeo::xs_geometry()`.
- The discharge table omits Drainage Area when it is unavailable.
- Local DEM uses only the signed adjacent-section slope at the selected cross
  section.
- A negative Local DEM slope remains visible as a local-scale observation but
  is not converted to an absolute value, clamped, or replaced with another
  cross section's positive slope.
- Zero, negative, non-finite, or missing slopes are never passed to the Manning
  calculation.
- If no valid slope exists, the discharge cards show an explanatory table
  instead of throwing an error.
- No zero, guessed, or sentinel drainage area is substituted.

## Dependency and compatibility

This behavior requires `fluvgeo >= 2026.07.25.9000`. The backend must be
released before this client is deployed from its configured `*release` remote.

Focused backend tests protect required, optional, and skipped watershed lookup
modes. OHWM tests deterministically protect immediate USGS success, transient
failure followed by success, exhausted retries, missing responses, DEM
fallback, and complete discharge unavailability. Live services are not a
requirement for the regression suite.
