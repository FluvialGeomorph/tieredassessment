# Parameterized Help

## Purpose
This module governs how a `golem`-based Shiny app implements contextual help as structured, reusable, package-managed data rather than as ad hoc prose scattered through UI and server code.

The goal is to create a contextual help system that is:
- maintainable
- composable
- testable
- aligned with implemented app behavior
- reusable across help surfaces such as tabs, plots, tables, and input controls

This module is about:
- help-data architecture
- help-data storage conventions
- stable help IDs
- help composition and rendering
- help-surface roles
- help-specific CSS requirements
- schema implications of the help-data contract
- help maintenance and drift control

This module is not about:
- general Shiny UI architecture
- app-specific domain content beyond help-system structure
- generic CSS unrelated to help surfaces
- package-wide governance outside the help system

## When to use
Use this module when:
- the repository contains a `golem`-based Shiny app developed as an R package
- the app needs contextual help across multiple surfaces
- help content should be reusable and centrally managed
- the team wants the chat session to implement and maintain a structured help system rather than writing one-off inline help text

This module is especially appropriate when:
- help appears in popovers, sidebars, tables, plots, or tab-level guidance
- the app exposes many derived variables or technical outputs
- the help system must stay aligned with changing package functionality over time

## Assumptions
This module assumes:

- the app is built with `golem`
- the app is an R package
- help content is stored as package data
- runtime helper functions are provided by `reproducibleai` and imported into the app package rather than copied into the app repository by default
- user-facing contextual help is part of the app’s functional interface and should be treated as a maintained system, not as incidental prose

## Scope and boundaries
This module governs:
- the structure of help data
- the storage and generation of help data
- the use of stable IDs as the contract between help content and UI references
- the composition of help records into UI surfaces
- the distinction between overview help and granular help
- help-specific CSS support for complex popovers
- integrity checking and drift review
- documentation of the help-data schema as a maintained structural contract

This module does not govern:
- the app’s domain logic
- the overall navigation structure of the app
- non-help-related CSS or theming
- general package governance
- broad user documentation such as README or pkgdown articles except where those docs must explain the help framework itself

## Required architecture

### 1. Help content must be structured data
Contextual help must be managed as structured data rather than inline prose in UI/server code.

Required fields:
- `id`
- `title`
- `summary`
- `detail`

These fields form the canonical help-data contract.

### 2. Help data must be created in `data-raw/`
Author and maintain help content in:

- `data-raw/create_help_data.R`

This script should generate the canonical help dataset.

### 3. Help data must be stored as package data
Persist help data as package data in:

- `data/help_data.rda`

Document the dataset in:

- `R/help_data.R`

### 4. Help-data schema must be documented
Because the help system defines a maintained structured interface, its schema should also be reflected in the repository’s schema documentation.

At minimum, the help-data contract should be represented in:

- `dev/40_schemas.md`

This helps prevent drift between:
- help data
- helper functions
- UI references
- expected required fields

### 5. Help IDs are the contract
Each help record must have a stable `id`.

UI and server code must reference help by ID rather than embedding duplicated prose.

IDs should be:
- stable over time
- semantically meaningful
- specific enough to support reuse
- updated deliberately during refactors

### 6. Help must be composable
The help system must support composing multiple help records into a single help surface.

Use:
- overview records for surfaces such as tabs, plots, and tables
- granular records for variables, quantities, metrics, methods, and concepts

Do not duplicate the same explanation across multiple surfaces when composition can be used instead.

### 7. Rendering should use reusable helpers
Use `reproducibleai` runtime helpers for composing and rendering help.

Preferred pattern:
- `reproducibleai::compose_help_items()`
- `reproducibleai::render_help_items()`

or imported equivalents in the app package namespace.

Do not copy these helpers into app repositories unless there is a deliberate reason to fork or customize behavior.

### 8. Help surfaces must have distinct roles
Different help surfaces should serve different purposes.

Recommended roles:
- getting-started / intro help: workflow orientation
- tab/sidebar help: decision framing and what to inspect
- plot help: what the plot shows and how to interpret it
- table help: what the table contains and how to use it
- granular variable help: definitions, formulas, units, and interpretation

Do not make every help surface a full technical essay.

### 9. Help popovers must support long-form content
If composed help is displayed in popovers or similar constrained containers, the app must include help-specific CSS that supports:
- wider popovers
- scrollable popover bodies
- readable long-form content

Help content must remain usable when multiple records are composed into one UI surface.

### 10. Formula rendering should be conservative
When mathematical notation is needed:
- prefer block MathJax
- avoid fragile inline math where possible
- keep formulas aligned with implemented code and units

### 11. Help must describe implemented behavior
Help text must describe what the app currently computes and displays.

Do not document:
- aspirational functionality
- intended future behavior
- domain formulas that differ from the implemented code without explicitly saying so

If implementation and theory differ, the help should describe the implementation clearly.

## Required conventions

### Help record categories
Use consistent categories of records such as:
- inputs
- intermediate or derived variables
- plot overview records
- table/scenario overview records
- tab/help-surface overview records
- reusable conceptual records

### Summary vs detail
Use:
- `summary` for concise orientation
- `detail` for fuller explanation

Do not collapse everything into `detail` unless the specific UI surface truly only needs one field.

### Naming and phrasing
Use consistent terminology for:
- units
- variable names
- adopted vs candidate outputs
- mass vs volume vs quantity
- method names

Prefer alignment with the app’s actual column names and outputs.

## Required chat behavior

### During implementation
When building or extending a help system, the chat must:
1. establish or verify the structured help-data architecture first
2. recommend or use `reproducibleai` scaffolding where appropriate
3. create help records as reusable data rather than inline prose
4. compose help records into surfaces rather than duplicating content
5. distinguish overview records from granular records
6. recognize when the help-data structure itself needs a schema update in `dev/40_schemas.md`

### During review and refinement
The chat must:
1. verify that referenced help IDs exist
2. identify missing records for newly exposed outputs or concepts
3. identify orphaned records that remain useful but unwired
4. identify overlap or redundancy across help surfaces
5. recommend concise improvements where help surfaces are doing the wrong job
6. check whether schema documentation remains aligned with the help-data contract

### During maintenance
The chat must:
1. update help after code changes that affect displayed behavior, formulas, units, or output names
2. flag stale help after renames or refactors
3. propose paste-ready updates when drift is detected
4. keep help aligned with what the app currently renders and computes
5. update schema documentation when required help fields or structural assumptions change

## Maintenance triggers
Update help data and/or help composition when:
- a new variable, output, plot, or table surface is added
- a displayed field is renamed
- a formula or unit convention changes
- a help composition vector changes
- a plot begins showing different metrics than before
- a tab’s role changes
- a refactor introduces missing or stale IDs

Update `dev/40_schemas.md` when:
- the required help-data fields change
- additional maintained fields are added
- helper assumptions about help record structure change
- the shape of the maintained help dataset changes materially

## Integrity and drift checks
A parameterized help system should be maintained with explicit checks such as:
- referenced IDs exist in `help_data`
- IDs are unique
- required columns exist
- help composition reflects currently displayed outputs
- formulas, units, and descriptions match the implemented code
- schema documentation reflects the actual maintained help-data structure

The chat should surface these checks during review work and provide paste-ready fixes when mismatches are found.

## Anti-patterns
Avoid the following:

### 1. Hard-coded help prose spread across the app
Do not scatter long help text directly through UI/server code when it should be part of structured help data.

### 2. Unstable or ad hoc IDs
Do not treat IDs casually. They are the contract between help content and UI references.

### 3. Duplicating the same explanation everywhere
Prefer composition and reusable records.

### 4. Treating all help surfaces as equivalent
Each surface should have a distinct role and level of detail.

### 5. Letting help drift from implementation
Do not leave formulas, units, names, or plot descriptions stale after code changes.

### 6. Leaving the help-data contract undocumented
If the application depends on a maintained structured help dataset, record that contract in `dev/40_schemas.md`.

### 7. Using long popovers without CSS support
If help content is rich or composed, ensure the UI remains readable.

### 8. Copying reusable runtime helpers into each app by default
Prefer importing `reproducibleai` helpers unless there is an intentional reason not to.

## Completion rule
A parameterized-help task is not complete if the session has:
- added or renamed help IDs
- changed help architecture
- changed displayed outputs or formulas
- altered help composition
- introduced new plotted or tabulated metrics

without also:
- updating the relevant help data
- checking help-ID integrity
- surfacing paste-ready updates for the user
- updating `dev/40_schemas.md` if the maintained help-data contract changed

## Relationship to other modules
This module is intended to be used alongside:
- package-development modules
- `golem`-specific Shiny modules
- development-governance modules

This module governs the contextual-help system only. It should remain separate from broader governance and broader Shiny architecture modules.

## Relationship to module handlers
This module remains a static reviewed instruction artifact.

In `reproducibleai`, instruction modules may have corresponding handler functions that install canonical instruction text and perform supporting repository configuration.

For `parameterized-help`, the handler may eventually support:
- scaffolding the help-data framework
- installing helper-oriented setup artifacts
- reinforcing consistent package-data conventions

The handler supports this module’s use, but does not replace the chat session’s responsibility to draft substantive app-specific help content and schema updates.
