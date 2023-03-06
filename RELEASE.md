# Release

## Instructions for Redeploying Try PureScript

After making a new compiler release, do the following to redeploy Try PureScript using the new compiler.

1. Submit a PR with the following changes:
    - In `stack.yaml`,
      - update the `resolver` to match the same one used in the PureScript repo
      - update `purescript` to use its new version.
    - Update the package set (see next section's instructions).
    - Update the shared config by running `cd client && npm run updateConfigVersions`.
    - Update the changelog to include the next release's date.
2. Once the PR is merged, create a new GitHub tagged release using `vYYYY-MM-DD.X` (where `X` is usually `1` or the release attempt) as the version schema. The release will trigger a GitHub Actions build.
3. Wait for the GitHub Actions build to finish (it builds the assets)
4. Run `./deploy/run.sh vX-X-X.1`, replacing `vX-X-X.1` with the version you created.

## Updating the Package Set

The try.purescript.org server only has a limited amount of memory. If the package set we use in deployment is too large, the server will run out of memory.

Before deploying an updated package set, someone (your reviewer) should check that the memory required to hold the package set's externs files does not exceed that of the try.purescript.org server.

Update the package set by doing the following. Each step is explained below:

### Summary

```sh
pushd staging
spago upgrade-set
cat > spago.dhall << EOF
{ name = "try-purescript-server"
, dependencies = [] : List Text
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
EOF
spago ls packages | cut -f 1 -d ' ' | xargs spago install
popd
pushd client
npm run updateConfigVersions
popd
# add any new shims
# update ES Module Shims (if needed)
```

### Step-by-Step Explanation

1. Update the `upstream` package set in `staging/packages.dhall`:

        ```
        $ pushd staging && spago upgrade-set && popd
        ```

2. Set the `dependencies` key in the `spago.dhall` file to be an empty list. This will require a type annotation of `List Text`:

        ```dhall
        { name = "try-purescript-server"
        , dependencies = [] : List Text
        , packages = ./packages.dhall
        , sources = [ "src/**/*.purs" ]
        }
        ```

3. For `staging/spago.dhall`, install all packages in the package set by running this command:

        ```
        $ spago ls packages | cut -f 1 -d ' ' | xargs spago install
        ```

4. Update the `client/src/Try/SharedConfig.purs` file by running this command in `client`:

        ```console
        $ npm run updateConfigVersions
        ```

5. If any packages need NPM dependencies, you can try adding their shims to the import map in `client/public/frame.html`
    - Open up the `generator.jspm.io` URL in the comment
    - Use the 'Add Dependency' search bar to find the NPM dependency
        - If it exists but doesn't exist in that CDN, you can try another one or [open an issue on `jspm/project`](https://github.com/jspm/project#issue-queue-for-the-jspm-cdn)
    - Update the version to the one you need once added
    - If needed, include other files from that dependency
    - Copy and paste the content into the `client/public/frame.html` file
    - Ensure `es-module-shims` has version `1.5.9` or greater.

6. If `es-module-shims` releases a new version, you can calculate its SHA-384 via

        ```console
        $ ESM_VERSION=1.5.5
        $ curl -L -o es-module-shims.js "https://ga.jspm.io/npm:es-module-shims@$ESM_VERSION/dist/es-module-shims.js"
        $ echo "sha384-$(openssl dgst -sha384 -binary es-module-shims.js | openssl base64 -A)"
        $ rm es-module-shims.js
        ```
