# ADR 0005: Release and synchronize customer applications from one upstream

- **Status:** Accepted
- **Date:** 2026-07-25
- **Deciders:** Maintainers of `FluvialGeomorph/ohwm2`
- **Builds on:** ADR 0004

## Context

The Ordinary High Water Mark, Floodplain Connectivity, and Tiered Assessment
applications share one geomorphic workflow but require independently reviewed
guidance, branding, deployment configuration, and release timing.

ADR 0004 established `ohwm2` as the canonical skinnable application template.
It did not define how downstream repositories preserve Git ancestry, consume
upstream releases, demonstrate readiness, or roll back a deployment. Without
that contract, maintainers must remember a long sequence of Git, dependency,
skin, test, manifest, and deployment steps for every customer.

## Decision

### Repository topology

- `fluvgeo` owns reusable scientific computations and is released before an
  `ohwm2` release that requires it.
- `ohwm2` owns the shared Shiny application, complete default skin, skin
  schema, tests, dependency state, and downstream tooling contract.
- Each customer application has a thin downstream repository with full Git
  ancestry from `ohwm2`.
- A downstream uses `origin` for its customer repository and `upstream` for
  `FluvialGeomorph/ohwm2`.
- The OHWM deployment will ultimately use a downstream repository too; the
  canonical upstream is not itself customer-owned.

### Ownership and permitted divergence

A downstream repository may own:

- `.fluvial-app.yml`;
- `inst/app/skin.yml`;
- customer assets below `inst/app/www/customer/`;
- generated deployment manifests and platform metadata;
- deployment secrets held outside Git.

Shared R code, tests, default assets, the skin loader and schema, `renv.lock`,
and renv activation infrastructure remain upstream-owned. A functional change
requested by one customer must be implemented in `ohwm2` or recorded as a new
architecture decision; it must not become an undocumented downstream patch.

### Release and synchronization units

- Downstreams consume immutable, published `ohwm2` releases, never an
  unreviewed tip of `main`.
- A published tag is never moved or reused.
- Each downstream records its approved upstream release in
  `.fluvial-app.yml`.
- Synchronization uses a dedicated `sync/<release>` branch and a merge that
  preserves upstream ancestry. Long-lived downstream history is not rebased.
- A merge conflict in an upstream-owned path is a stop condition until its
  ownership and intended resolution are understood.

### Verification and promotion

- A read-only verification command will enforce metadata, Git lineage, skin,
  asset, dependency, test, and manifest preconditions.
- The first tooling release reports evidence and blocks on failed required
  checks; it does not fetch, merge, tag, deploy, or resolve conflicts.
- Staging and production use the same verified downstream commit.
- Production promotion is an explicitly approved action.
- Rollback redeploys the previous known-good downstream commit and manifest;
  it does not move a release tag or rewrite Git history.

## Consequences

### Positive

- Shared application improvements have one implementation and test surface.
- Customer presentation remains isolated from shared workflow code.
- Every deployment can identify its upstream release and exact downstream
  commit.
- Synchronization and rollback become repeatable and auditable.
- Read-only automation can remove skipped steps without gaining deployment
  authority.

### Costs and constraints

- Maintainers operate one upstream and three downstream repositories.
- The downstream metadata schema becomes a maintained interface.
- Customer assets must use the reserved customer asset directory.
- Upstream releases must include compatibility and migration notes when the
  skin or metadata contract changes.
- A customer-specific functional request may require upstream product design
  before that customer can deploy it.

## Follow-up

1. Define `.fluvial-app.yml` under `dev/schemas/`.
2. Maintain upstream release, downstream bootstrap, synchronization,
   promotion, and rollback workflows.
3. Implement a read-only downstream verification command with machine-readable
   output.
4. Pilot the contract with the OHWM downstream application before creating the
   other two customer repositories.
