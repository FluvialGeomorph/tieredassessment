# Development scripts

Store maintained automation supporting development workflows here. Scripts should document inputs, outputs, dependencies, and safe execution expectations.

- `01_start.R`: one-time golem/package initialization history.
- `02_dev.R`: maintained interactive development setup, context validation,
  environment checks, local application launch, tests, and CI helpers.
- `03_deploy.R`: package checks, dependency snapshot, application audit, and
  deployment preparation.
- `run_dev.R`: lightweight local application launcher.

Run these scripts selectively; they are development runbooks, not an automated
sequence. Never commit values for environment variables or deployment secrets.
