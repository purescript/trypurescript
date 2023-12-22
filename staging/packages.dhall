let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.13-20231219/packages.dhall
        sha256:35b9271b0a49390a9681995c609dbf7357402a1f209e0549d840bca295abe57b

in  upstream
  with metadata.version = "v0.15.13"
