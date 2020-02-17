Try PureScript
--------------

[Try PureScript](https://try.purescript.org) is an online PureScript code editor for quickly experimenting with PureScript code snippets and ideas.

## How to Add a New Backend

- Create a `bower.json`/`psc-package.json` file with all the dependencies you want to be available for the library backend and optionally an extra `Try` module if you want. [Example PR](https://github.com/purescript/trypurescript/pull/84/files)
- Write an example in a github gist and name it `Main.purs`. [Example Gist](https://gist.github.com/ff1e87f0872d2d891e77d209d8f7706d)
- add a backend button and a backend option in `index.html` and `index.js` respectively. [Example PR](https://github.com/purescript/trypurescript/pull/85/files)
    - Note to link the gist from the previous bullet in the `mainGist` field in `index.js`
- Ask phil to update the `core` main gist to link to the new backend


## Features:

- Writing code using the [Ace Editor](http://ace.c9.io)
- Automatic compilation
- PureScript syntax highlighting
- Run and print output or show resulting JavaScript
- Multiple view modes: code, output or both
- Persistent session
- Load PureScript code from Github Gists
- Save PureScript code as anonymous Github Gists
    - (_Note: These Gists are not associated with your GitHub account and are visible to anyone with a link to them_)


## Control Features via the Query String

Most of these features can be controlled not only from the toolbar, but also using the [query parameters](https://en.wikipedia.org/wiki/Query_string):

- **Load From Gist**: Load PureScript code from Gist id using the `gist` parameter
    - Example: `gist=37c3c97f47a43f20c548` will load the code from this Gist if the file was named `Main.purs`

- **View Mode**: Control the view mode using the `view` parameter
    - Options are: `code`, `output`, `both`
    - Example: `view=output` will only display the output

- **Backend**: Control which backend will compile your code using the `backend` parameter
    - Options are: `core`, `thermite`, `slides`, `flare`, `mathbox`, `behavior`
    - Example: `backend=thermite` will use the thermite backend

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

## Packages

- The packages set and compiler version for Try-PureScript can be viewed [here](https://github.com/purescript/trypurescript/tree/master/staging/core/psc-package.json).

- The packages set and compiler version for Try-Thermite can be viewed [here](https://github.com/paf31/try-thermite/blob/gh-pages/staging/psc-package.json).


