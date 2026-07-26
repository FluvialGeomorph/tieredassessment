# Verifying a downstream application repository

## Current command

Run this command from a clean downstream repository after the recorded
upstream release tag has been fetched:

```r
evidence <- ohwm2::validate_downstream_repository(
  file = ".fluvial-app.yml",
  repository = "."
)
```

The command is read-only. It returns an invisible, machine-readable R list
containing the validated metadata, downstream `HEAD`, resolved upstream release
commit, and downstream-changed paths.

The current implementation validates:

- the version 1 `.fluvial-app.yml` structure;
- a clean Git working tree;
- the canonical URL of the configured `upstream` remote;
- local resolution of the recorded immutable release tag;
- ancestry from that release commit to downstream `HEAD`;
- confinement of changes since that release to downstream-owned paths.

The command does not fetch or prove remote publication by itself. Fetch the
canonical remote as an explicit synchronization step, and review the published
release before treating a local tag as authoritative.

## Current manual checks

Until later tooling increments are implemented, continue to run these checks
separately:

- `ohwm2::validate_app_skin_file("inst/app/skin.yml")`;
- referenced customer-asset review;
- `renv::status()` and dependency-source review;
- focused and complete tests, `R CMD check`, and an interactive workflow test;
- manifest regeneration and comparison for the exact downstream commit;
- staging, promotion approval, production smoke testing, and deployment
  recordkeeping.

Failure of the current command is a stop condition. Success is necessary but
is not yet sufficient for deployment.

## Planned increments

The verifier will be extended without gaining mutation authority:

1. compose skin and customer-asset checks;
2. report dependency and manifest evidence;
3. represent each check in a stable machine-readable report;
4. add a command-line entry point suitable for CI;
5. add release mode after all required checks have deterministic evidence.

It will not fetch, merge, tag, resolve conflicts, generate manifests, deploy,
or promote releases.
