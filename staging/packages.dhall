let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210823/packages.dhall sha256:501660806a34bb3b0293648673bba4457628d1cbc250dbdc2fca2a031ded1294

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
