#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const packageJson = require("../../package.json");

const filesToCopy = ["LICENSE", "README.md", "tailwind.json"];

function mkdirpSync(p) {
  if (fs.existsSync(p)) {
    return;
  }
  mkdirpSync(path.dirname(p));
  fs.mkdirSync(p);
}

function removeSync(p) {
  fs.rmSync(p, {force:true, recursive: true});
}

const src = path.resolve(path.join(__dirname, "../../"));
const dst = path.resolve(path.join(__dirname, "../../", "_release"));

removeSync(dst);
mkdirpSync(dst);

for (const file of filesToCopy) {
  const p = path.join(dst, file);
  mkdirpSync(path.dirname(p));
  fs.copyFileSync(path.join(src, file), p);
}

fs.copyFileSync(
  path.join(src, ".github", "ci-scripts", "postinstall.js"),
  path.join(dst, "postinstall.js")
);

// create an empty file as a placeholder to be replaced by a symlink at postinstall
const filesToTouch = ["sx_ppx"];
for (const file of filesToTouch) {
  const p = path.join(dst, file);
  mkdirpSync(path.dirname(p));
  fs.writeFileSync(p, "");
}

const pkgJson = {
  name: packageJson.name,
  version: packageJson.version,
  description: packageJson.description,
  homepage: packageJson.homepage,
  license: packageJson.license,
  repository: packageJson.repository,
  scripts: {
    postinstall: "node postinstall.js",
  },
  bin: {
    "sx_ppx": "sx_ppx",
  },
  files: [
    "platform-linux-x64/",
    "platform-darwin-x64/",
    "postinstall.js",
    "tailwind.json",
    "sx_ppx",
  ],
};

fs.writeFileSync(
  path.join(dst, "package.json"),
  JSON.stringify(pkgJson, null, 2)
);