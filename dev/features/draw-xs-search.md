# Draw XS location search

The Draw XS map provides OpenStreetMap/Nominatim location search through
`leaflet.extras`.

The Leaflet Search formatter boundary is normalized locally because released
client bundles have used both of these invocation contracts:

- `formatData(response)`
- `formatData(control, response)`

The Draw XS formatter accepts both forms, ignores malformed results, and uses
`display_name` (falling back to `name`) as the visible suggestion label. This
prevents results from rendering as the literal `undefined` when the bundled
plugin passes its control object as the first argument.

Regression coverage in `tests/testthat/test-draw-xs-search.R` verifies the
formatter contract and its inclusion in the Draw XS map.
