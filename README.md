# Try PureScript

[![Build Status](https://api.travis-ci.org/purescript/trypurescript.svg?branch=master)](http://travis-ci.org/purescript/trypurescript)

[Try PureScript](https://try.purescript.org) is an online PureScript code editor for quickly experimenting with PureScript code snippets and ideas. It consists of a client and a server component, both of which live within this repository.

## Features:

- Writing code using the [Ace Editor](http://ace.c9.io)
- Automatic compilation
- PureScript syntax highlighting
- Run and print output or show resulting JavaScript
- Multiple view modes: code, output or both
- Persistent session
- Load PureScript code from Github Gists

### Which Libraries are Available?

Try PureScript aims to provide a complete, recent package set from <https://github.com/purescript/package-sets>. The available libraries are those listed in `staging/spago.dhall`, at the versions in the package set mentioned in `staging/packages.dhall`.

To update to a more recent package set, first update the `upstream` package set in `staging/packages.dhall`, and then run:

```
$ spago ls packages | cut -f 1 -d ' ' | xargs spago install
```

to install every package in the set. Before deploying an updated package set, someone should check that the memory required to hold the package set's externs files does not exceed that of the try.purescript.org server.

### Control Features via the Query String

Most of these features can be controlled not only from the toolbar, but also using the [query parameters](https://en.wikipedia.org/wiki/Query_string):

- **Load From Gist**: Load PureScript code from Gist id using the `gist` parameter
    - Example: `gist=37c3c97f47a43f20c548` will load the code from this Gist if the file was named `Main.purs`

- **View Mode**: Control the view mode using the `view` parameter
    - Options are: `code`, `output`, `both` (default)
    - Example: `view=output` will only display the output

- **Auto Compile**: Automatic compilation can be turned off using the `compile` parameter
    - Options are: `true` (default), `false`
    - Example: `compile=false` will turn auto compilation off

- **JavaScript Code Generation**: Print the resulting JavaScript code in the output window instead of the output of the program using the `js` parameter
    - Options are: `true`, `false` (default)
    - Example: `js=true` will print JavaScript code instead of the program's output

- **Session**: Load code from a session which is stored with [localStorage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) using the `session` parameter
    - Usually managed by Try PureScript
    - Example: `session=9162f098-070f-4053-60ea-eba47021450d` (Note: will probably not work for you)
    - When used with the `gist` query parameter the code will be loaded from the Gist and not the session

## Development

### 1. Client setup

```sh
git clone git@github.com:purescript/trypurescript.git
cd trypurescript/client

npm install
npm run build

cd public
httpserver 8080 #eg with: alias httpserver='python -m SimpleHTTPServer'
open http://localhost:8080
```

### 2. Local compile server setup

```sh
git clone git@github.com:purescript/trypurescript.git
cd trypurescript

stack build

# Install PureScript dependencies
cd staging
spago install

stack exec trypurescript 8081 $(spago sources)
# should output that is is compiling the sources (first time)
# then: Setting phasers to stun... (port 8081) (ctrl-c to quit)
```

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

Response body on other errors (eg, the request body was too large)

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
