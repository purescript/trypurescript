#!/usr/bin/env node

// This script expects the current working directory to be `client`.
// Call it using:
//   node updateSharedConfigVersions.mjs src/Try/SharedConfig.purs
//
// It keeps `SharedConfig.purs` in sync with the two sources of truth:
//   - the PureScript compiler version, taken from the server's `stack.yaml`
//     (the `purescript-X.Y.Z` extra-dep, i.e. the compiler that actually
//     compiles user snippets);
//   - the registry package set version, taken from `staging/spago.yaml`
//     (the `registry:` field). With Spago v1 the package set is a registry
//     version and no longer encodes a purs version, so the two are read
//     independently. The footer URLs are derived from these values inside
//     `SharedConfig.purs`, so only the version strings are written here.

import fs from "fs";
import path from "path";
import process from "process";

if (process.argv.length <= 2) {
  throw new Error("Script was run with 0 args. The first and only arg should be the path to the 'SharedConfig.purs' file.")
}

const sharedConfigPath = process.argv[2];

const stackYamlPath = path.join("..", "stack.yaml");
const stagingSpagoYamlPath = path.join("..", "staging", "spago.yaml");
const stackYamlContent = fs.readFileSync(stackYamlPath, "utf-8");
const stagingSpagoYamlContent = fs.readFileSync(stagingSpagoYamlPath, "utf-8");

const pursVersion = stackYamlContent.split("\n")
  .reduce((acc, nextLine) => {
    if (acc.found) return acc;
    const matchResult = nextLine.match(/ +- purescript-(.+)/);
    return matchResult
      ? { found: true, value: matchResult[1] }
      : acc;
  }, { found: false })
  .value;

const packageSetVersionMatch = stagingSpagoYamlContent.match(/registry:\s*(\S+)/);
const packageSetVersion = packageSetVersionMatch ? packageSetVersionMatch[1] : undefined;

if (!pursVersion) {
  throw new Error("Failed to extract the PureScript version from the stack.yaml file. Cannot update SharedConfig.purs file.");
}

if (!packageSetVersion) {
  throw new Error("Failed to extract the registry package set version (the 'registry:' field) from the staging/spago.yaml file. Cannot update SharedConfig.purs file.");
}

const sharedConfigContent = fs.readFileSync(sharedConfigPath, "utf-8");
const newContent = sharedConfigContent.split("\n")
  .map((line) => line
    .replace(/pursVersion =.*/, `pursVersion = "v${pursVersion}"`)
    .replace(/packageSetVersion =.*/, `packageSetVersion = "${packageSetVersion}"`)
  )
  .join("\n");
fs.writeFileSync(sharedConfigPath, newContent);
