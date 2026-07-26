# Promoting and rolling back a downstream application

## Trigger

Use this workflow after a downstream synchronization or customer-only
presentation change has passed automated verification.

## Inputs

- verified downstream commit;
- generated manifest for that commit;
- staging and production deployment targets;
- previous known-good production commit;
- reviewer approval.

## Staging

1. Require a clean working tree and record `git rev-parse HEAD`.
2. Run the downstream verification command in release mode.
3. Generate or confirm the manifest from that exact commit and dependency
   state.
4. Deploy the commit to staging.
5. Verify customer presentation and the complete workflow in a fresh session.
6. Record the staging URL or target identifier outside secrets-bearing files.
7. Obtain explicit promotion approval.

## Production promotion

1. Confirm `HEAD` and the manifest checksum still match the staged evidence.
2. Deploy the same commit and manifest to production.
3. Run a concise production smoke test.
4. Record deployment time, downstream commit, upstream release, R version, and
   manifest checksum.
5. Preserve the prior known-good deployment record for rollback.

## Stop conditions

Stop promotion when:

- any tracked file changed after staging verification;
- staging and production would use different commits or manifests;
- a required secret or external service is unavailable;
- the customer workflow fails;
- the previous known-good commit is unknown.

## Rollback

1. Select the previous known-good downstream commit and its manifest.
2. Redeploy that immutable pair to production.
3. Run the production smoke test.
4. Record the rollback and the failed release evidence.
5. Diagnose and fix the issue through normal upstream or downstream ownership
   rules.

Do not move a release tag, rewrite branch history, or create an unreviewed code
revert during an operational rollback.

## Durable outputs

- staging and production deployment records;
- exact commit and manifest provenance;
- preserved rollback target;
- incident or follow-up record when rollback was required.
