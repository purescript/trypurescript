trypurescript
=============

Very basic browser-based interface to the PureScript compiler.

## API

### Compile PureScript code

- Method: `POST`
- Endpoint: `/compile/text`
- Request body: PureScript code
- Response body: Compiled Javascript code or error string
- Status code: 200 (success) or 500 (error)

