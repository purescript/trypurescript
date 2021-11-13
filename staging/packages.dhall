let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211111/packages.dhall sha256:7ed6350fe897a93926d16298e37d2324aabbe5eca99810204719dc3632fb555f

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
