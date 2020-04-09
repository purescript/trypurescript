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

bower install
npm install
npm run build
npm run bundle

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
psc-package install

# note: globs like **/src/** do not work
stack exec trypurescript 8081 ".psc-package/psc-0.13.6-*/*/*/src/**/*.purs" "src/*.purs"
# should output that is is compiling the sources (first time)
# then: Setting phasers to stun... (port 8081) (ctrl-c to quit)
```

## Server API

The server is a very basic web service which wraps the PureScript compiler, allowing clients to send PureScript code to be compiled and receiving either compiled JS or error messages in response.

### Compile PureScript code

**POST /compile**

- Request body: PureScript code
- Response body: Either `{ js: "..." }` or `{ error: "..." }`
- Status code: 200 (success)

Note that if the code in the request body fails to compile, this is considered a success from the perspective of the API, so compilation failures will be returned with 2xx status codes.
Among other things, this makes it easier to use the API from another domain using CORS.

The output code will contain references to preloaded modules using `require` calls.
To run these files in the browser, it is necessary to either use a `require` shim (such as require1k), or replace these calls and deploy a bundle of precompiled modules.
The Try PureScript client uses the second approach.

### Configuration

The server application takes the following arguments on the command line:

- port number
- a list of input source files

#### Example

    dist/build/trypurescript/trypurescript 8081 'bower_components/purescript-*/src/**/*.purs'
