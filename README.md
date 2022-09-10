# Try PureScript

[![Build Status](https://github.com/purescript/trypurescript/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/trypurescript/actions?query=workflow%3ACI+branch%3Amaster)

[Try PureScript](https://try.purescript.org) is an online PureScript code editor for quickly experimenting with PureScript code snippets and ideas. It consists of a client and a server component, both of which live within this repository.

## Features:

- Writing code using the [Ace Editor](http://ace.c9.io)
- Automatic compilation
- PureScript syntax highlighting
- Run and print output or show resulting JavaScript
- Multiple view modes: code, output or both
- Shareable code and editor state via URL
- Load PureScript code from GitHub Gists or repository files

### Control Features via the Query String

Most of these features can be controlled not only from the toolbar, but also using the [query parameters](https://en.wikipedia.org/wiki/Query_string):

- **Load From GitHub Repo**: Load a PureScript file from a GitHub repository using the `github` parameter
    - Format: `github=/<owner>/<repo>/<branch or tag>/<file>.purs`
    - Example: `github=/purescript/trypurescript/master/client/examples/Main.purs`.
    - Notes: the file should be a single PureScript module with the module name `Main`.

- **Load From Gist**: Load PureScript code from a gist using the `gist` parameter
    - Format: `gist=<gist id>`
    - Example: `gist=37c3c97f47a43f20c548`
    - Notes: the file should be named `Main.purs` with the module name `Main`.

- **Load From URL**: Load compressed PureScript code using the `code` parameter
    - Managed by Try PureScript and updated on editor state change to create shareable URLs
    - Format: `code=<compressed string>`
    - Example: `code=LYewJgrgNgpgBAWQIYEsB2cDuALGAnGIA` will set the editor state to the single line `module Main where`

- **View Mode**: Control the view mode using the `view` parameter
    - Options are: `code`, `output`, `both` (default)
    - Example: `view=output` will only display the output

- **Auto Compile**: Automatic compilation can be turned off using the `compile` parameter
    - Options are: `true` (default), `false`
    - Example: `compile=false` will turn auto compilation off

- **JavaScript Code Generation**: Print the resulting JavaScript code in the output window instead of the output of the program using the `js` parameter
    - Options are: `true`, `false` (default)
    - Example: `js=true` will print JavaScript code instead of the program's output

### Which Libraries Are Available?

Try PureScript aims to provide a complete, recent package set from <https://github.com/purescript/package-sets>. The available libraries are those listed in [`staging/spago.dhall`](./staging/spago.dhall), at the versions in the package set mentioned in [`staging/packages.dhall`](./staging/packages.dhall).

## Development

### 1. Shared setup

These steps should be performed whether you are working on the server, the client, or both.

```sh
# Clone into the repository
git clone git@github.com:purescript/trypurescript.git
cd trypurescript
```

### 2. Local compile server setup

This step sets up a local server for Try PureScript. You can skip this step if you just want to use the client with the production server.

```sh
# Build the trypurescript executable
stack build

# Set up the PureScript environment for the server
cd staging
spago build

# Ensure the compiled JavaScript is available to the client via symbolic link.
ln -s "$PWD/output" "$PWD/../client/public/js/output"

# Then, start the server.
#
# Below, we disable glob expansion via `set -o noglob` to ensure that globs are
# passed to `purs` unchanged.
#
# We run this in a subshell so that setting noglob only lasts for the duration
# of the command and no longer.
(set -o noglob && stack exec trypurescript 8081 $(spago sources))

# Should output that it is compiling the sources (first time)
# Then: Setting phasers to stun... (port 8081) (ctrl-c to quit)
```

### 3. Client setup

```sh
# Install development dependencies
cd client
npm install

# Use `serve:dev` if you are using a local Try PureScript server,
# e.g. you followed the instructions in step 1.
#
# Use `serve:production` if you would like
# to test the client against the production Try PureScript server.
# Note: the production server may not match the package set you have locally.
npm run serve:(dev|production)

# Try PureScript is now available on localhost:8080
```

### 4. Choosing a Tag

The built-in examples for Try PureScript are loaded from this GitHub repository. To change the tag that the examples are loaded from, you'll need to touch three files:

* `client/config/dev/Try.Config.purs`
* `client/config/prod/Try.Config.purs`
* `client/examples/Main.purs`, in the `fromExample` function.

If you are preparing a release or if you need to adjust examples in development, you should change the tag in these three places (and ensure you're using the same tag in each place!).

## Server API

The server is a very basic web service which wraps the PureScript compiler, allowing clients to send PureScript code to be compiled and receiving either compiled JS or error messages in response.
It is hosted at <https://compile.purescript.org/>.

### Compile PureScript code

#### POST /compile

- Request body: PureScript code defining a module whose name must be Main
- Status code: 200 (success)

Response body on compilation success:

```javascript
{
  "js": "...", // a string containing JavaScript code
  "warnings": [ ... ] // an array of warnings, using the same format as the
                      // compiler's --json-errors flag
}
```

Response body on compilation failure:

```javascript
{
  "error": {
    "tag": "CompilerErrors",
    "contents": [ ... ] // an array of errors, using the same format as the
                        // compiler's --json-errors flag
  }
}
```

Response body on other errors (eg, the name of the module in request body was not Main, or the request body was too large)

```javascript
{
  "error": {
    "tag": "OtherError",
    "contents": "..." // a string containing an error message
  }
}
```

Note that the API returns a 200 response in all of the above cases; in particular, if the code in the request body fails to compile and the API returns errors, this is still considered a success.
Among other things, this makes it easier to use the API from another domain using CORS.

The output code will contain references to any imported modules using `require` calls.
To run these files in the browser, it is necessary to either use a `require` shim (such as require1k), or replace these calls and deploy a bundle of precompiled modules.
The Try PureScript client uses the first approach.

#### GET /output/:module/(index.js|foreign.js)

The server exposes the compiled JS for all of the modules it has access to.
If the compiled JavaScript code in the response includes a `require` call such as `require(../Web.HTML/index.js)`, then the client is expected to arrange things so that this `require` call provides access to the JavaScript code available at the URL path `/output/Web.HTML/index.js`, via a shim or otherwise.

### Configuration

The server application takes the following arguments on the command line:

- port number
- a list of input source files

#### Example

    trypurescript 8081 'bower_components/purescript-*/src/**/*.purs'
