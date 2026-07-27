# Publish a customer application through Posit Connect Cloud

## Purpose

Use this procedure to update an existing FluvialGeomorph customer application
hosted by Posit Connect Cloud. It preserves the existing content item and
public URL.

Organization:
`https://connect.posit.cloud/fluvialgeomorph`

## Production mapping

| Application | GitHub repository | Required public URL |
|---|---|---|
| Ordinary High Water Marks | `FluvialGeomorph/ohwm` | `https://fluvialgeomorph-ohwm.share.connect.posit.cloud/` |
| Floodplain Connectivity | `FluvialGeomorph/floodplainconnectivity` | `https://fluvialgeomorph-floodplainconnectivity.share.connect.posit.cloud/` |
| Tiered Assessment | `FluvialGeomorph/tieredassessment` | `https://fluvialgeomorph-tieredassessment.share.connect.posit.cloud/` |

## Before the first migration

Open the existing Connect Cloud content item and record, without recording
secret values:

- content name, owner, and public URL;
- connected GitHub repository and branch;
- Shiny framework and application subdirectory;
- R runtime;
- access controls;
- environment-variable names;
- automatic-build or redeployment behavior;
- previous successful deployment and Git commit, when displayed.

If the repository, content identity, or branch relationship is unclear, stop
before changing GitHub.

## Prepare the deployment revision

In the customer repository:

1. Merge the reviewed bootstrap, synchronization, or customer-skin PR.
2. Pull `main` and confirm Source Control is clean.
3. Run `check_downstream_repository()`, the required tests and package check,
   and the complete interactive workflow.
4. Generate `manifest.json` with the documented dependency-resolution mode.
5. Record:

   ```powershell
   git rev-parse HEAD
   git remote get-url origin
   Get-FileHash -Algorithm SHA256 manifest.json
   git status --short
   ```

Do not deploy when the status command reports a changed file.

## Update the existing content item

1. Sign in to the FluvialGeomorph Connect Cloud organization as a named
   publisher.
2. Open the existing content item whose public URL matches the production
   mapping.
3. Confirm its GitHub source is the matching customer repository and reviewed
   deployment revision.
4. Use the existing content item's supported update or redeploy action with
   the **Shiny** framework.
5. Review the build log and confirm it uses the intended repository commit and
   manifest.

Do not continue from an organization-level **Publish** flow if it will create
a second content item or different public URL. Stop and review the existing
content settings instead.

Connect Cloud credentials, environment values, and access settings are
external state. Never copy them into Git, logs, screenshots committed to the
repository, or deployment records.

## Production acceptance

In a fresh browser session:

1. Open the required public URL and confirm it is unchanged.
2. Confirm the expected customer title, guidance, theme, and assets.
3. Complete Draw XS, Draw Flowline, and Results.
4. Confirm the expected scientific outputs and responsive slider behavior.
5. Record the deployed customer commit, manifest hash, Connect build result,
   reviewer, and previous known-good deployment.

## Rollback

Use the existing content item's supported rollback or redeployment mechanism
to restore the previous known-good customer commit and matching manifest.
Confirm the public URL is unchanged and repeat the production smoke test.

Do not move an `ohwm2` release tag, rewrite Git history, force-push customer
`main`, or create a replacement content item as an operational rollback.

## Hosted geospatial binary ABI failures

If a compiled R package loads unsuccessfully because a shared-library SONAME
such as `libproj.so.*` is absent, distinguish the package's declared system
requirements from the libraries against which its installed binary was built.
Connect Cloud is managed: do not prescribe `apt` commands or edit a customer
manifest by hand.

Retry the same existing-content build once. If the identical loader error
recurs, retain the failed log and public URL, submit them through Connect
Cloud's in-app support option, and repair the dependency in `ohwm2`. When the
package otherwise supports Connect Cloud's documented system libraries, prefer
an immutable upstream source commit for the same package version so the hosted
build compiles against its own runtime ABI. Publish and validate a new `ohwm2`
release before synchronizing customer repositories.

## Stop conditions

Stop and obtain review when:

- the existing content item or required URL cannot be identified;
- the GitHub repository or deployment revision differs from the recorded
  customer commit;
- Connect Cloud proposes a new public URL;
- the build uses an unexpected R version or dependency source;
- the build, public access, customer skin, or scientific smoke test fails;
- the previous known-good deployment cannot be identified.
