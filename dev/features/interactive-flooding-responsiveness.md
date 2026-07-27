# Interactive flooding responsiveness

## User-visible behavior

The Channel REM and Floodplain REM sliders provide two coordinated update
lanes:

- map polygons follow slider movement through a 120 ms throttled stream, and
- plots, classification, storage volume, and discharge follow a 400 ms
  debounced stream after the slider settles.

While analytical outputs are pending, Results displays a compact status message.
Map polygon replacement preserves the user's current center and zoom.

Repeated calculations are accelerated without changing the scientific
definitions:

- a bounded least-recently-used cache stores up to 32 water-surface polygons
  and is shared by Channel and Floodplain for identical REM values,
- one precomputed threshold and cumulative-sum lookup returns the same storage
  volume as `fluvgeo::floodplain_volume()` without resampling or scanning the
  raster for every slider value, and
- immutable base cross-section points feed separate Channel and Floodplain
  classified views, so each settled slider invalidates only its own spatial
  relationship and dependent outputs.

Changing the selected cross section recalculates both REM slider bounds from
the cached base points and clamps the current values into the new valid range.
This requires no DEM, polygon, or slope recomputation.

## Reactive ownership

Results outputs are registered once for the Shiny session. Slider observers
update reactive geometry, classification, and volume state; they do not
replace plot or table renderers. Full water-surface raster objects are not
retained because the outputs consume cached polygons and the exact volume
lookup directly.

`channel_polygon_level` and `floodplain_polygon_level` record the elevation
represented by each live map polygon. The settled analytical path reuses that
polygon when it already matches the final slider value and otherwise resolves
the final polygon before calculating dependent outputs.

The scientific definitions remain owned by `fluvgeo`. Deterministic tests
compare the cached volume and single-field spatial classification against
`fluvgeo::floodplain_volume()` and `fluvgeo::xs_pts_classify()`. Polygon cache
misses call `fluvgeo::water_surface_poly()` unchanged.

## Deferred optimization boundary

This pass does not change polygon construction or limit classification to only
the visible cross section. Client-side raster thresholding and selected-XS-only
classification remain possible future optimizations if larger sites require
them.
