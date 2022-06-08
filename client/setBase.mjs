#!/usr/bin/env node

import path from "path";
import fs from "fs";
import process from "process";

const filePath = path.join("public", "frame.html");

// PureScript generates JS with the following import lines:
//    `import * as Data_Foo from "../Data.Foo/index.js"
// To remap `../Data.Foo/index.js` to `output/Data.Foo/index.js`
// we append `/ignored/`.
//
// This:    `/output/ignored/../Data.Foo/index.js`
// becomes: `/output/Data.Foo/index.js`
const prodPath = "/output/ignored/"
const devPath = "/js/output/ignored/"

const environment = process.argv[2] || "dev";
const baseHref = environment === "prod" ? prodPath : devPath;

const frameHtml = fs.readFileSync(filePath, "utf-8");
const newHtml = frameHtml
  .split("\n")
  .map((line) => line.replace(/^( *<base href=")[^"]*(".+)$/, `$1${baseHref}$2`))
  .join("\n");

fs.writeFileSync(filePath, newHtml);
