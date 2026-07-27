# Editable workflow and DEM request guardrails

## User-visible behavior

The workflow supports backward navigation and correction:

1. **Draw XS** is the authoritative cross-section editor.
2. Selecting **Draw Flowline** snapshots exactly the cross sections currently
   present in that editor, assigns fresh sequential identifiers, validates the
   requested terrain extent, and retrieves a DEM for that snapshot.
3. A successful XS/DEM submission creates a new Flowline editor generation.
   This prevents observers and geometry from an earlier terrain map from
   participating in the new pass.
4. Selecting **View Results** snapshots the flowline currently present in the
   active Flowline editor and recomputes all downstream geometry from the raw
   XS snapshot and current flowline.
5. Returning from Results directly to Draw Flowline preserves that editor, so
   flowline additions, edits, and deletions are reflected on the next Results
   submission.

Submitting revised cross sections intentionally creates a fresh Flowline
editor because the terrain extent and raster may have changed. The user must
draw the flowline on that current terrain generation before Results is
available.

## State ownership

- `xs_geometry_snapshot` owns raw cross-section linework for the active
  terrain generation.
- `flowline_geometry_snapshot` records the raw flowline most recently
  submitted to Results.
- Processed `xs`, `fl`, `xs_pts`, REM, water surfaces, volumes, plots, and
  slope caches are downstream artifacts and never become editor inputs.
- A forward submission invalidates Results readiness and slope caches before
  recomputation.
- Flowline editor module IDs are generation-specific. The app never recreates
  a module under an already-used Shiny ID.

## DEM request policy

Messaging alone is not sufficient for a request that is predictably invalid.
The app therefore uses three layers:

1. **Local prevention:** reject empty/invalid linework, geometry outside the
   supported Web Mercator map, and buffered requests larger than the app's
   configured small-site span.
2. **Service validation:** treat service errors that indicate missing extent
   or data as no coverage, and validate that a returned raster contains at
   least one finite elevation.
3. **Recovery messaging:** preserve the XS editor state and explain whether
   the user should reduce/move the site or retry after a service failure.

The default maximum buffered request span is 10,000 metres. Deployments may
set `options(ohwm2.max_dem_span_m = <positive metres>)` before constructing
the app. This is an application resource policy, not a claim that the remote
provider has one permanent universal request limit.

The upstream image service also advertises properties such as spatial extent,
`maxImageWidth`, and `maxImageHeight`. Those service-side values remain
authoritative for an individual request. The local policy exists to prevent
obviously unsuitable small-site requests before authentication and transfer.

## Failure behavior

- Oversized request: remain on Draw XS and explain the requested and allowed
  spans.
- Outside supported map: remain on Draw XS and request relocation.
- Service no coverage or an all-missing raster: remain on Draw XS and explain
  that usable terrain is unavailable.
- Authentication, timeout, connection, or other service failure: remain on
  Draw XS, preserve the geometry, and invite a retry.
- Invalid Results geometry: preserve the editors, leave Results unready, log
  technical details, and show a nontechnical correction message.
