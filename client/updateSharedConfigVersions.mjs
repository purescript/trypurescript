#!/usr/bin/env node

// This script expects the current working directory to be `client`.
// Call it using:
//   node updateSharedConfigVersions.mjs src/Try/SharedConfig.purs

import fs from "fs";
import path from "path";
import process from "process";

if (process.argv.length <= 2) {
  throw new Error("Script was run with 0 args. The first and only arg should be the path to the 'SharedConfig.purs' file.")
}

const sharedConfigPath = process.argv[2];

const stackYamlPath = path.join("..", "stack.yaml");
const stagingPackagesDhallPath = path.join("..", "staging", "packages.dhall");
const stackYamlContent = fs.readFileSync(stackYamlPath, "utf-8");
const packagesContent = fs.readFileSync(stagingPackagesDhallPath, "utf-8");

// The `purescript` extra-dep is either a Hackage dependency, e.g.
//   - purescript-0.15.13
// or a GitHub dependency pinned to a release tag, e.g.
//   - github: purescript/purescript
//     commit: v0.15.16
const pursVersion = (() => {
  const lines = stackYamlContent.split("\n");
  for (let i = 0; i < lines.length; i++) {
    const hackageMatch = lines[i].match(/ +- purescript-(.+)/);
    if (hackageMatch) return hackageMatch[1];
    if (/ +- github: purescript\/purescript\s*$/.test(lines[i])) {
      const commitMatch = (lines[i + 1] ?? "").match(/ +commit: v(.+)/);
      if (commitMatch) return commitMatch[1];
    }
  }
  return undefined;
})();

const packageSetVersion = packagesContent
  .match(/https:\/\/github.com\/purescript\/package-sets\/releases\/download\/psc-([^\/]+)\/packages.dhall/)[1];

if (!pursVersion) {
  throw new Error("Failed to extract the PureScript version from the stack.yaml file. Cannot update SharedConfig.purs file.");
}

if (!packageSetVersion) {
  throw new Error("Failed to extract the Package Set version from the staging/packages.dhall file. Cannot update SharedConfig.purs file.");
}

const sharedConfigContent = fs.readFileSync(sharedConfigPath, "utf-8");
const newContent = sharedConfigContent.split("\n")
  .map((line) => line
    .replace(/pursVersion =.*/, `pursVersion = "v${pursVersion}"`)
    .replace(/packageSetVersion =.*/, `packageSetVersion = "${packageSetVersion}"`)
  )
  .join("\n");
fs.writeFileSync(sharedConfigPath, newContent);
