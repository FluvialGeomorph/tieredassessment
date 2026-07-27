# Results without watershed-service dependency

## User-visible behavior

After users draw cross sections and a flowline, the Results transition does not
block on remote watershed delineation or USGS stream-network enrichment. The
workflow continues through REM creation, cross-section stationing,
classification, plots, and volume calculation before the reach-slope lookup
runs.

USGS NHDPlus reach slope is preferred for the Manning calculation. The app:

- defaults the **Slope scale** control to **USGS Reach (recommended)** while
  allowing **Sampled DEM Reach** and **Local XS Neighborhood** exploration,
- bounds remote requests with a timeout and retry/backoff policy,
- uses a fast NHDPlus catchment-coverage lookup rather than a potentially
  long-running raindrop trace,
- treats a location with no NHDPlus COMID as a terminal coverage result,
  immediately skips retries, and explains when the Sampled DEM Reach fallback
  is being used,
- caches USGS Reach and Sampled DEM Reach once for all cross sections,
- calculates and caches the complete Local XS Neighborhood profile in one pass,
- retries the single USGS Reach lookup when the user requests it,
- falls back to Sampled DEM Reach when USGS is unreachable or returns no
  coverage and the sampled reach slope is positive,
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
- Sampled DEM Reach uses `(max(Z) - min(Z)) / profile length`, where `Z` and
  `POINT_M` come from the flowline points plotted in the longitudinal profile.
- Local XS Neighborhood uses only the signed adjacent-section thalweg slope
  centered at the selected cross section.
- A negative Local XS Neighborhood slope remains visible as a local-scale
  observation but
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
fallback, all three scale definitions, bulk local-profile caching, and complete
discharge unavailability. Live services are not a requirement for the
regression suite.
