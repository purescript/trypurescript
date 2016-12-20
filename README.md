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
