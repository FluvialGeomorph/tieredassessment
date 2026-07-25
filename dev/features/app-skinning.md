# Application skinning

## User-visible capability

One shared Shiny application can present task-specific identity and guidance
for Ordinary High Water Mark, Floodplain Connectivity, and Tiered Assessment
work without changing its geomorphic workflow.

## Current behavior

At startup, the app:

1. loads the complete OHWM-compatible defaults;
2. discovers an optional downstream override;
3. recursively merges the override over the defaults;
4. normalizes and validates the result;
5. injects that immutable skin into UI and server construction.

Override discovery order is:

1. an explicit `run_app(skin_file = ...)` path;
2. `FLUVIAL_APP_SKIN_FILE`;
3. packaged `inst/app/skin.yml`;
4. no override.

The current skin surface includes:

- navigation and browser identity;
- Bootswatch theme and Bootstrap version;
- favicon;
- workflow navigation and sidebar labels;
- instruction lists;
- next-step button labels;
- long-running transition messages.

## Compatibility behavior

With no override, the rendered app retains the existing OHWM presentation.
Visible tab labels may change, but server navigation continues to use the stable
values `draw_xs`, `draw_flowline`, and `results`.

## Non-goals

Skinning does not:

- change calculations;
- skip or reorder workflow stages;
- enable features;
- alter validation rules;
- provide arbitrary HTML or executable configuration;
- replace deployment secrets or operational configuration.

See `dev/schemas/app-skin.md` for the exact contract and ADR 0004 for the
architectural decision.
