# Agentic-context migration 0.1

- Applied: 2026-07-24
- Profiles: base, r-package
- Policy: replacements were created and validated before tracked legacy sources were removed.

## Adapted artifacts

- `dev/05_plan.md` -> `dev/goals/project-plan.md`
- `dev/10_design.md` -> `dev/architecture/design.md`
- `dev/20_testing.md` -> `dev/workflows/shiny-workflow-testing.md`
- Shiny/golem conventions were distilled into
  `dev/workflows/shiny-golem-development.md`.
- Cross-repository ownership rules were recorded in
  `dev/workflows/cross-repository-development.md`.
- The unimplemented parameterized-help proposal was classified as deferred in
  `dev/features/parameterized-help.md`; it is not treated as current behavior.
- The four golem development scripts were moved from `dev/` into
  `dev/scripts/`, and their internal paths and agentic-context calls were
  updated.

## Removed artifacts

- `dev/instructions/chat-manual.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/CHAT_INSTRUCTIONS.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/development-governance.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/goals.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/parameterized-help.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/r-package.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/instructions/shiny-golem.md`: Legacy instruction copy is replaced by AGENTS.md and routed durable artifacts.
- `dev/sessions/.backups/2026-06-23_backup_20260623_175536.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026-05-31-arch-review.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026-05-31.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026-06-23-slider-observer-refactor.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026-06-23.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026_05_31-results-tab-reactive-debugging.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.
- `dev/sessions/2026_05_31-results-workflow-testing-checkpoint.md`: Session transcript is non-authoritative; durable state belongs in maintained artifacts and Git preserves history.

## Verification

- Replacement files copied: 2
- Superseded files removed: 16
- The standard structure passed validation before removal.
- Git history remains the archive for superseded content.
- The legacy Shiny, testing, and project-priority semantics were reviewed and
  routed into maintained artifacts before removal.
- The available R 4.6.1 library did not include `testthat`, `devtools`, or the
  application dependency stack, so package tests could not start in the
  migration shell. This is an environment limitation, not a recorded passing or
  failing test result.
