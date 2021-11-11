# Release

## Instructions for Redeploying Try PureScript

After making a new compiler release, do the following to redeploy Try PureScript using the new compiler.

1. Submit a PR with the following changes:
    - In `stack.yaml`, update `purescript` (and possibly `purescript-cst`) to use its new version.
2. Once the PR is merged, create a new GitHub tagged release using `vYYYY-MM-DD.X` (where `X` is usually `1` or the release attempt) as the version schema. The release will trigger a GitHub Actions build.
3. Wait for the GitHub Actions build to finish (it builds the assets)
4. Run `./deploy/run.sh vX-X-X.1`, replacing `vX-X-X.1` with the version you created.
