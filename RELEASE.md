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

To check, run the server locally from `staging/` with the production RTS flags (see `deploy/start`) plus `-s`, which prints a heap summary when the server exits:

```console
$ cd staging
$ rm -rf .psci_modules   # start cold: the server's compilation cache
$ (set -o noglob && stack exec trypurescript -- +RTS -N2 -A128m -M3G -s -RTS 8081 $(spago sources))
```

Cold boot compiles the whole set, which takes a few minutes. Once the server responds on port 8081, stop the server with Ctrl-C and you should see a summary printed on exit. The number to watch is `bytes maximum residency`. You'll have to run this a _second time_, keeping the `.psci_modules` that the first run produced, to measure the peak memory usage (warm boot has higher max live heap than cold boot without cache).

Update the package set by doing the following. Each step is explained below:

### Summary

```sh
pushd staging
cat > spago.yaml << EOF
package:
  name: try-purescript-server
  dependencies: []
workspace:
  packageSet:
    registry: 0.0.1
  extraPackages: {}
EOF
spago upgrade
spago install $(spago ls packages --json --quiet \
  | jq -r 'to_entries[] | select(.value.type == "registry") | .key' \
  | grep -vxF -f <(awk -F'\t' '!/^#/ && NF {print $1}' excluded-packages.txt))
popd
pushd client
npm run updateConfigVersions
popd
# add any new shims
# update ES Module Shims (if needed)
```

### Step-by-Step Explanation

1. Overwrite `staging/spago.yaml` with a placeholder config: an empty
   `dependencies` list and any valid registry package set as a seed (the exact
   version doesn't matter — the next step overwrites it). Starting from a clean
   file ensures packages dropped from the new set don't linger.

2. Upgrade to the latest package set:

    ```console
    $ spago upgrade
    ```

    This rewrites `workspace.packageSet.registry` to the newest available set,
    replacing the seed version. Each set's JSON at
    <https://github.com/purescript/registry/tree/main/package-sets> records the
    `compiler` version it targets, which should line up with the `purescript`
    version in `stack.yaml` — pin the set explicitly in step 1 instead if the
    latest set targets a compiler you don't want.

3. Install every package in the new set — except those listed in
   `staging/excluded-packages.txt` (see [Excluded Packages](#excluded-packages))
   — so they're all available in the playground. This overwrites the empty
   `dependencies` list in `staging/spago.yaml`, then downloads, compiles, and
   locks them.

    ```console
    $ spago install $(spago ls packages --json --quiet \
        | jq -r 'to_entries[] | select(.value.type == "registry") | .key' \
        | grep -vxF -f <(awk -F'\t' '!/^#/ && NF {print $1}' excluded-packages.txt))
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

### Excluded Packages

`staging/excluded-packages.txt` lists the packages deliberately left out of the
set, one per line with a tab-separated reason. The install command above
filters through this file, so re-running the update steps keeps them excluded.

The only exclusion criterion is that a package cannot *run* in the playground:
its foreign modules (or a dependency's) import bare JS specifiers — npm packages
or Node builtins — that the import map in `client/public/frame.html` does not
shim, so the compiled code throws as soon as it executes. Excluding these
packages also keeps the server inside its memory budget (see the note at the top
of this section).

To bring an excluded package back: add shims for its imports to the import map
(step 5 above), then remove its line — along with the lines of any packages
that were excluded only for depending on it — and re-run the install step.

When upgrading to a new package set, check any packages that are new to the
set for unshimmed bare imports in their foreign modules, and append them here
with a reason.
