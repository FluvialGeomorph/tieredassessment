# Releasing the canonical application upstream

## Trigger

Use this workflow when a reviewed `ohwm2` change is ready to become an
immutable synchronization point for downstream customer applications.

## Inputs

- a clean `main` branch synchronized with `origin/main`;
- completed pull requests and release notes;
- the intended package version and release tag;
- the required published `fluvgeo` release;
- a supported R runtime and synchronized renv library;
- successful package and interactive workflow verification.

## Ordered actions

1. If scientific behavior changed, publish and verify the required `fluvgeo`
   release first.
2. Create a release-preparation branch from current `main`.
3. Update `DESCRIPTION`, `NEWS.md`, generated package documentation, README
   version metadata, and the `fluvgeo` lock record.
4. Hydrate or restore the renv library, then require `renv::status()` to report
   a consistent project.
5. Run focused tests, the complete test suite, `R CMD check`, and the full
   interactive Draw XS through Results workflow.
6. Generate the deployment manifest using the repository's documented
   dependency-resolution mode.
7. Review the Git diff, dependency sources, skin-schema changes, and migration
   notes.
8. Merge the release-preparation pull request into `main`.
9. Tag the resulting merge commit. Never tag an earlier branch commit when the
   merge changes the commit graph.
10. Push the tag and publish a GitHub Release with the relevant `NEWS.md`
    content.
11. Verify installation through `@*release` and record the resolved commit.

## Stop conditions

Stop the release when:

- a required backend release is missing;
- the working tree or renv state is inconsistent;
- tests, package checks, or the interactive workflow fail;
- the manifest contains a local or otherwise unreproducible package source;
- a required skin migration is undocumented;
- the proposed tag already exists.

## Verification

The release record must demonstrate:

- release tag and commit;
- package version;
- required `fluvgeo` version and source;
- skin-schema version;
- R version;
- test and package-check results;
- successful `@*release` installation.

## Durable outputs

- immutable Git tag;
- published GitHub Release;
- updated release notes and dependency state;
- an upstream release suitable for downstream synchronization.
