# Release

## Instructions for Redeploying Try PureScript

After making a new compiler release, do the following to redeploy Try PureScript using the new compiler.

1. Submit a PR with the following changes:
    - In `stack.yaml`, update `purescript` (and possibly `purescript-cst`) to use its new version.
    - If you are updating the package set, see the next section for instructions.
1. Once the PR is merged, create a new GitHub tagged release using `vYYYY-MM-DD.X` (where `X` is usually `1` or the release attempt) as the version schema. The release will trigger a GitHub Actions build.
2. Wait for the GitHub Actions build to finish (it builds the assets)
3. Run `./deploy/run.sh vX-X-X.1`, replacing `vX-X-X.1` with the version you created.

## Updating the Package Set

The try.purescript.org server only has a limited amount of memory. If the package set we use in deployment is too large, the server will run out of memory.

Before deploying an updated package set, someone (your reviewer) should check that the memory required to hold the package set's externs files does not exceed that of the try.purescript.org server.

Update the package set by doing the following:

1. Update the `upstream` package set in `staging/packages.dhall`:

        ```
        $ pushd staging && spago upgrade-set && popd
        ```

2. Set the `dependencies` key in the `spago.dhall` file to be an empty list. This will require a type annotation of `List Text`:

        ```dhall
        { name = "try-purescript-server"
        , dependencies = [] : List Text
        , ...
        }
        ```

3. Install all packages in the package set by running this command:

        ```
        $ spago ls packages | cut -f 1 -d ' ' | xargs spago install
        ```
