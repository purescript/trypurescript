let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260701/packages.dhall
        sha256:bb62858371c55d439baa6bcf463ab70cfe8753d5e1f0520b0955468c018213d6

in  upstream
  with metadata.version = "v0.15.16"
