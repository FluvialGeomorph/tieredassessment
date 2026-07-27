# ADR 0006: Deploy customer applications through Posit Connect Cloud

- **Status:** Accepted
- **Date:** 2026-07-26
- **Deciders:** Maintainers of `FluvialGeomorph/ohwm2`
- **Builds on:** ADR 0005

## Context

The three customer applications are hosted as separately configured Shiny
content items in the FluvialGeomorph Posit Connect Cloud organization. Each
content item is sourced from its customer GitHub repository. Its public URL is
already in use and must survive the migration to the shared `ohwm2` lineage.

Local `rsconnect` deployment metadata is intentionally absent. Treating old
`rsconnect::deployApp()` scripts as authoritative would create a second
deployment path and could replace a stable public URL.

## Decision

- Posit Connect Cloud is the production host for the customer applications.
- Each existing Connect Cloud content item remains the production identity for
  its application.
- Existing public URLs are release invariants:

  | Application | GitHub repository | Public URL |
  |---|---|---|
  | Ordinary High Water Marks | `FluvialGeomorph/ohwm` | `https://fluvialgeomorph-ohwm.share.connect.posit.cloud/` |
  | Floodplain Connectivity | `FluvialGeomorph/floodplainconnectivity` | `https://fluvialgeomorph-floodplainconnectivity.share.connect.posit.cloud/` |
  | Tiered Assessment | `FluvialGeomorph/tieredassessment` | `https://fluvialgeomorph-tieredassessment.share.connect.posit.cloud/` |

- Connect Cloud builds the Shiny application from the matching customer
  GitHub repository and its reviewed deployment revision.
- A named publisher uses the authenticated Connect Cloud interface. Tokens,
  environment values, and other secrets remain outside Git.
- A migration or release updates the existing content item. It must not create
  a replacement item merely because the source repository history changed.
- The GitHub migration, downstream validation, and successful new `main` must
  be complete before a production build is initiated.
- Production publication and rollback remain explicit human approval gates.

## Consequences

### Positive

- Public URLs and application identity remain stable across shared-core
  releases.
- GitHub contains the reviewable source and manifest for each deployment.
- Local workstations do not become the authoritative deployment source.
- Customer applications can adopt the same upstream release independently.

### Costs and constraints

- Repository, branch, framework, access, runtime, and automatic-build settings
  must be inspected in Connect Cloud before the first migration.
- A GitHub default-branch migration must minimize the interval in which the
  expected new `main` does not exist.
- Connect Cloud settings and deployment history are external operational state
  and cannot be reconstructed from Git alone.
- A new content item or changed public URL requires a separate reviewed
  decision.

## Operational procedure

Follow
[Publish a customer application through Posit Connect Cloud](../workflows/posit-connect-cloud-deployment.md).
