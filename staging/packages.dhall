let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260506/packages.dhall
        sha256:c3ffa9beb6cca64d4fd1de91cbeec48d61002defaaf769952372f94dbdeb7b2a

in  upstream
  with metadata.version = "v0.15.15"
