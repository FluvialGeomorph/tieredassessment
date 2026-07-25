# Parameterized help

## Status

Deferred; not implemented in `ohwm2`.

The retired instruction set described a possible structured contextual-help
system, but the repository does not currently contain its proposed data-raw
generator, package dataset, dataset documentation, runtime helpers, or
help-specific stylesheet. That proposal is therefore not a current application
contract and must not be treated as implemented behavior.

## Reconsideration trigger

Reconsider this feature when contextual help becomes an active product
requirement. Before implementation:

1. confirm the user-facing help surfaces and ownership of reusable runtime
   helpers;
2. record the accepted data contract under `dev/schemas/`;
3. use stable, unique help IDs and centrally managed package data;
4. test missing, duplicate, referenced, and orphaned IDs;
5. keep descriptions, formulas, units, and output names aligned with implemented
   application behavior;
6. provide readable styling for any long or composed help surface.

Until that decision is made, add only task-specific concise help using existing
application conventions; do not silently introduce the retired speculative
framework.
