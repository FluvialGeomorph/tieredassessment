# Agent instructions

## Identity and scope

`ohwm2` is the current repository. Treat its code, tests, configuration, and maintained documentation as the authoritative evidence for repository-local behavior.

## Always-applicable rules

- Inspect repository and Git evidence before making consequential changes.
- Keep this file concise; route detailed knowledge into maintained artifacts under `dev/`.
- Preserve unrelated user changes and keep work within the requested repository scope.
- Distinguish verified evidence, reasonable inference, and unknowns.

## Conditional context routes

- Goals, scope, or success criteria: `dev/goals/`
- Architecture, dependencies, or ownership boundaries: `dev/architecture/`
- Consequential and durable choices: `dev/decisions/`
- Governance or artifact lifecycle: `dev/governance/`
- Repeatable development procedures: `dev/workflows/`
- Exact structural contracts: `dev/schemas/`
- Cohesive user-visible capabilities: `dev/features/`
- Resumable unfinished work: `dev/checkpoints/current/`
- R package API, implementation, documentation, or tests: `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `tests/testthat/`, and `dev/workflows/r-package-development.md`
- Shiny or golem architecture, reactivity, configuration, or deployment: `dev/workflows/shiny-golem-development.md`
- Application skins, presentation configuration, or downstream synchronization: `dev/features/app-skinning.md`, `dev/schemas/app-skin.md`, `dev/schemas/downstream-app-metadata.md`, and `dev/workflows/downstream-skin-synchronization.md`
- Upstream releases, downstream repository topology, deployment promotion, or rollback: `dev/decisions/adr-0005-downstream-release-topology.md` and the applicable procedure under `dev/workflows/`
- Operator-facing customer repository procedures: begin with `dev/workflows/downstream-operator-index.md` and `dev/workflows/downstream-repository-basics.md`
- Downstream verification behavior and current automation boundary: `dev/workflows/downstream-verification.md`
- Reactive transition or regression testing: `dev/workflows/shiny-workflow-testing.md`
- Current deadline work, including Draw XS hardening: `dev/goals/project-plan.md`
- Changes crossing the `ohwm2` and `fluvgeo` boundary: `dev/workflows/cross-repository-development.md`

Full session transcripts are not normal context sources. Use maintained durable artifacts and concise checkpoints.

## Completion governance

Before declaring meaningful work complete, determine whether it changed goals, architecture, a durable decision, a schema or interface, a repeatable workflow, feature behavior, or resumable state. Update the applicable durable artifact. Create a checkpoint only when useful unfinished state remains.

## Verification and information governance

Run checks proportionate to the change, inspect Git status and diff, and confirm the change boundary. Never record secrets, credentials, PII, restricted data, or unnecessarily large logs in agentic-context artifacts.
