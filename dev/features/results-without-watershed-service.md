# Results without watershed-service dependency

## User-visible behavior

After users draw cross sections and a flowline, the Results transition no
longer blocks on remote watershed delineation. The workflow continues through
REM creation, cross-section stationing, classification, plots, volume
calculation, and Manning discharge calculation.

## Data behavior

- `Watershed_Area_SqMile` remains present and numeric.
- Its value is missing because the OHWM workflow explicitly skips watershed
  enrichment.
- DEM-derived area, width, and depth are calculated through
  `fluvgeo::xs_geometry()`.
- The discharge table omits Drainage Area when it is unavailable.
- No zero, guessed, or sentinel drainage area is substituted.

## Dependency and compatibility

This behavior requires `fluvgeo >= 2026.07.25.9000`. The backend must be
released before this client is deployed from its configured `*release` remote.

Focused backend tests protect required, optional, and skipped lookup modes.
OHWM tests protect geometry and discharge preparation with missing drainage
area, and the full OHWM suite exercises the external DEM and NHD paths.
