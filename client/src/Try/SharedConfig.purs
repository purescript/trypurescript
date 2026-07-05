-- | Similar to the `Try.Config` module
-- | except values below can be used in both
-- | the dev and production environments
module Try.SharedConfig where

import Prelude

pursVersion :: String
pursVersion = "v0.15.15"

pursReleaseUrl :: String
pursReleaseUrl = "https://github.com/purescript/purescript/releases/tag/" <> pursVersion

-- | The registry package set version used by `staging/spago.yaml`.
packageSetVersion :: String
packageSetVersion = "77.10.2"

packageSetPackageJsonUrl :: String
packageSetPackageJsonUrl = "https://github.com/purescript/registry/blob/main/package-sets/" <> packageSetVersion <> ".json"
