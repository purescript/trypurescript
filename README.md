Try PureScript
--------------

[Try PureScript](https://try.purescript.org) is an online PureScript code editor for quickly experimenting with PureScript code snippets and ideas.

### Features:

- Writing code using the [Ace Editor](http://ace.c9.io)
- Automatic compilation
- PureScript syntax highlighting
- Run and print output or show resulting JavaScript
- Multiple view modes: code, output or both
- Persistent session
- Load PureScript code from Github Gists
- Save PureScript code as annonymous Github Gists
    - (_Note: These Gists are not associated with your GitHub account and are visible to anyone with a link to them_)


### Control Features via the Query String

Most of these features can be controlled not only from the toolbar, but also using the [query parameters](https://en.wikipedia.org/wiki/Query_string):

- **Load From Gist**: Load PureScript code from gist id using the `gist` parameter
    - Example: `gist=37c3c97f47a43f20c548`

- **View Mode**: Control the view mode using the `view` parameter
    - Options are: `code`, `output`, `both`
    - Example: `view=output`

- **Auto Compile**: Automatic compilation can be turned off using the `compile` parameter
    - Options are: `true`, `false`
    - Example: `compile=false` will turn auto compilation off

- **JavaScript Code Generation**: Print the resulting JavaScript code in the output window instead of the output of the program using the `js` parameter
    - Options are: `true`, `false`
    - Example: `js=true` will print JavaScript code instead of the program's output

- **Session**: Load code from a session which is stored with [localStorage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) using the `session` parameter
    - Usually managed by Try PureScript
    - Example: `session=9162f098-070f-4053-60ea-eba47021450d` (Note: will probably not work for you)
    - When used with the `gist` query parameter the code will be loaded from the Gist and not the session

### Packages

The packages set and compiler version for Try PureScript can be viewed [here](https://github.com/purescript/trypurescript/tree/master/staging/core/psc-package.json).