# PureScript API

[![Build Status](https://api.travis-ci.org/purescript/trypurescript.svg?branch=master)](http://travis-ci.org/purescript/trypurescript)

Very basic web service which wraps the PureScript compiler.

[Client code](https://github.com/purescript/trypurescript/tree/gh-pages)

## API

### Compile PureScript code

**POST /compile**

- Request body: PureScript code
- Response body: Either `{ js: "..." }` or `{ error: "..." }`
- Status code: 200 (success)

The response does not use error codes, to make it easier to use the API from another domain using CORS.

The output code will contain references to preloaded modules using `require` calls. To run these files in the browser, it is necessary to either use a `require` shim (such as require1k), or replace these calls and deploy a bundle of precompiled modules (the Try PureScript client uses the second approach).

## Configuration

The application takes the following arguments on the command line:

- port number
- a list of input source files

### Example

    dist/build/trypurescript/trypurescript 8081 'bower_components/purescript-*/src/**/*.purs'
    
# Development

## 1. Client setup

```
git clone git@github.com:purescript/trypurescript.git
cd trypurescript
git co gh-pages

bower install
npm install
npm run build
npm run bundle

httpserver 8080 #eg with: alias httpserver='python -m SimpleHTTPServer'
open http://localhost:8080
```

## 2. Work with local compile server

```
git clone git@github.com:purescript/trypurescript.git
cd trypurescript

stack build

# use one of the backends
cd staging/core
# get the sources of the deps
psc-package install

# note: globs like **/src/** do not work
stack exec trypurescript 8081 ".psc-package/psc-0.13.6/*/*/src/**/*.purs" "src/*.purs"
# should output that is is compiling the sources (first time)
# then: Setting phasers to stun... (port 8081) (ctrl-c to quit)
```

## 3. Point client to local compile server

(instead of the ones at try.purescript.org)
```
# edit API.purs

  , compile: compile "http://localhost:8081"
  , getBundle: getDefaultBundle "http://localhost:8081"

```
