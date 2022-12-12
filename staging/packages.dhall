let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221212/packages.dhall
        sha256:62ea94a2728dc4f0a47491c064776f402a8a1b3cf2d8a7ec83c96075d30bd590

in  upstream
  with metadata.version = "v0.15.7"
