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

The application takes three arguments on the command line:

- externs path
- externs config file
- port number

### Externs Path

This directory should contain externs files for all modules which should be precompiled on startup. The name of each file in the directory should be `Module.Name.json`.

### Externs Config File

An example file is provided in `conf/core.conf`. A config file consists of a list of module names, one per line, topologically sorted by module dependencies and including all trasitive dependencies. The easist way to create this file is to copy the output of `psc` after a fresh compile.

### Example

    dist/build/trypurescript/trypurescript externs/ conf/core.conf 8081
