# shiny-golem — Overlay Module (Shiny apps using {golem})

## Canonical guidance (required)
- Treat **Engineering Production-Grade Shiny Apps** as canonical guidance for building Shiny apps with {golem}: https://engineering-shiny.org/
- Treat **{golem}** as the chosen framework for this session unless the user explicitly opts out.
- When providing advice, prefer the approach most aligned with the book + golem conventions; explicitly call out deviations.

## Scope & assumptions (required)
Confirm early (do not infer):
1) deployment target (Posit Connect / shinyapps.io / Shiny Server / container / other),
2) authn/authz requirements (if any),
3) expected concurrency + performance constraints,
4) data sources (files/DB/APIs) and secrets management,
5) whether the app is internal vs external-facing,
6) whether the app must work offline / in disconnected environments.

## Core principle: the app is a package (required)
- A golem app is an **R package**. All `r-package` expectations apply:
  - clean DESCRIPTION/NAMESPACE,
  - documented exported API where appropriate,
  - tests,
  - `R CMD check` as a quality gate.
- Prefer package-oriented thinking:
  - stable “entry points”,
  - explicit dependencies,
  - minimal global side effects.

## Recommended golem structure (required)
When discussing structure, default to these conventions unless repo evidence suggests otherwise:
- **Entrypoint**:
  - `run_app()` as the main launch function (accepting config parameters where appropriate).
- **UI/server split**:
  - `app_ui()` defines/assembles UI,
  - `app_server()` defines server logic.
- **Modules-first**:
  - implement features as Shiny modules with paired `mod_<name>_ui()` and `mod_<name>_server()`.
  - keep module files small and cohesive; prefer multiple modules over one large server function.
- **Non-reactive domain logic**:
  - place pure functions (data transforms, scoring, business rules) in regular `R/` functions **outside** reactive contexts.
  - reactive code should orchestrate domain functions, not contain all business logic.

## Naming & organization conventions (required)
- Naming:
  - modules: `mod_<feature>_ui()` + `mod_<feature>_server()`
  - helpers: `<verb>_<noun>()` for pure functions; avoid ambiguous names like `process()` without a domain noun.
- Organization guidance:
  - one primary module per file when possible (or tightly related module pairs),
  - keep UI-building helpers separate from compute helpers if it improves testability,
  - avoid “misc.R” dumping grounds—prefer discoverable names.

## Reactivity discipline (required)
- Prefer explicit reactive boundaries:
  - use `reactiveVal()`/`reactiveValues()` intentionally and document ownership of state,
  - use `eventReactive()` / action-button triggers for expensive work,
  - avoid accidental invalidation storms (broad reactives feeding many outputs).
- Avoid global mutable state shared across sessions.
- For long tasks:
  - do not block the session unnecessarily; recommend patterns that provide progress and responsiveness (as supported by the team’s deployment environment).

## Robustness & UX (required)
- Validate inputs and fail safely:
  - use `validate()`/`need()` (or equivalent patterns) to prevent downstream errors,
  - show user-safe error messages; keep technical details in logs.
- Provide predictable UX:
  - loading/progress states for expensive operations,
  - clear empty/error states,
  - deterministic navigation and reset behavior.

## Dependencies, performance, and footprint (required)
- Be conservative with dependencies; justify each addition.
- Avoid heavy work inside render functions when it can be cached or precomputed.
- If data is large:
  - recommend caching strategy and invalidate rules,
  - prefer incremental updates over full recomputation where possible.

## Configuration, environments, and secrets (required)
- Configuration must be explicit and environment-aware (dev/test/prod).
- Never hardcode secrets in code or committed files.
- Recommend environment variables or platform secret stores; document required variables.
- Recommend least-privilege credentials for DB/API access.

## Testing strategy (required)
Require a test plan aligned to package + Shiny realities:
- Unit tests for pure domain logic (highest ROI).
- Tests for module logic where feasible (focus on deterministic behavior).
- Regression tests for critical user flows (what must never break).
- Prefer moving logic out of reactive contexts into pure functions to make testing possible.

## Deployment readiness checklist (required)
When asked about production/deployment, produce a checklist covering:
- `R CMD check` + tests passing in CI
- reproducible builds (explicit versions / lockfile strategy if used)
- environment variables and config documented
- logging/monitoring plan (what to log, where it goes)
- resource sizing assumptions (CPU/RAM), concurrency expectations
- known bottlenecks + mitigation plan

