# PureScript API

Very basic web service which wraps the PureScript compiler.

## API

### Compile PureScript code

**POST /compile**

- Request body: PureScript code
- Response body: Either `{ js: "..." }` or `{ error: "..." }`
- Status code: 200 (success)

The response does not use error codes, to make it easier to use the API from another domain using CORS.
