# ADR 0001: Repository Lineage, Architecture Assessment, and Alignment Strategy

**Date:** 2026-05-31  
**Status:** Accepted  

## Context

Three related golem-based Shiny apps exist in the FluvialGeomorph organization:
- `tieredassessment` — original proof-of-concept; semver; oldest fluvgeo pin; domain functions inline
- `ohwm` — second-generation app; calver; domain functions migrated to fluvgeo; logging/audit infrastructure added
- `ohwm2` — most recent; built from ohwm; sandstone theme; full `dev/` governance structure; most mature

All three serve slightly different audiences but share >90% of functionality (same workflow: Draw XS → Draw Flowline → View Results; same fluvgeo backend; same output surfaces).

These apps were built before AI-assisted workflows were adopted. The server architecture reflects an early learning curve: imperative `observeEvent` blocks, `<<-` global assignment for all reactive state, nested observers, and no formal reactivity discipline.

## Decisions

### 1. Repo lineage
`tieredassessment` → `ohwm` → `ohwm2` is the confirmed build order.  
`ohwm2` is the current most-mature app and the active development target.

### 2. Immediate strategy (Option A)
Formalize `ohwm` as the canonical upstream template.  
Stabilize `ohwm2` first, then backport improvements to `ohwm`.  
New apps (including `tieredassessment` replacement) are built from `ohwm`.  
Alignment between repos is maintained via AI-assisted manual diffing until CI workflow is warranted.

### 3. Medium/long-term strategy (Option B + D)
- Move all shared infrastructure functions into `fluvgeo` or a dedicated shared package (eliminates copy-paste drift for `empty_raster`, `empty_sf`, `log_message`, `run_app_audit`, etc.)
- Parameterize app identity (title, theme, audience-specific features) via `config.yml` so a single codebase can be deployed as multiple apps
- `tieredassessment` ArcGIS-specific logic (`arcgis_auth`, `rLFT`, `smoothr`) is audience-specific and should not be merged into the shared core

### 4. `tieredassessment` disposition
Nothing in `tieredassessment` needs to be retained. It will be replaced by a new app built from the stabilized `ohwm` template.

### 5. CI drift detection (Option C)
Deferred. AI-assisted manual diffing is sufficient while `ohwm2` is being stabilized. Revisit after the `ohwm` backport is complete.

## Consequences

- All active development happens in `ohwm2`
- `ohwm` is frozen until the backport phase
- `tieredassessment` is deprecated; no further investment
- The `dev/` governance structure currently only in `ohwm2` must be added to `ohwm` as part of the backport
