-- | Similar to the `Try.Config` module
-- | except values below can be used in both
-- | the dev and production environments
module Try.SharedConfig where

import Prelude

pursVersion :: String
pursVersion = "v0.15.13"

pursReleaseUrl :: String
pursReleaseUrl = "https://github.com/purescript/purescript/releases/tag/" <> pursVersion

packageSetVersion :: String
packageSetVersion = "0.15.13-20231219"

packageSetPackageJsonUrl :: String
packageSetPackageJsonUrl = "https://github.com/purescript/package-sets/blob/psc-" <> packageSetVersion <> "/packages.json"
