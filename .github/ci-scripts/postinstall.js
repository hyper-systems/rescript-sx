#!/usr/bin/env node

const path = require("path");
const cp = require("child_process");
const fs = require("fs");

const platform = process.platform;

function find_arch() {
  // If MacOS, use the x86_64 build for both x86_64 and arm64 (ran with rosetta 2) machines
  if (process.platform === "darwin") {
    return "x64";
  }

  if (process.arch === "x64") {
    return "x64";
  }

  if (process.platform === "linux") {
    const output = cp.execSync("getconf LONG_BIT", { encoding: "utf8" });
    return output === "64\n" ? "x64" : "x86";
  }

  return "x86";
}

const copyPlatformBinaries = (platformPath) => {
  const platformBuildPath = path.join(__dirname, platformPath);
  const sourcePath = path.join(platformBuildPath, "sx_ppx");
  const destPath = path.join(__dirname, "sx_ppx");
  if (fs.existsSync(destPath)) {
    fs.unlinkSync(destPath);
  }
  fs.chmodSync(sourcePath, 0o755);
  // link instead of copying
  fs.symlinkSync(sourcePath, destPath);
};

const arch = find_arch();
const platformPath = "platform-" + platform + "-" + arch;
const supported = fs.existsSync(platformPath);

if (!supported) {
  console.error("rescript-sx does not support this platform :(");
  console.error("");
  console.error("If you want rescript-sx to support this platform natively,");
  console.error(
    "please open an issue here: https://github.com/hyper-systems/rescript-sx/issues/new"
  );
  console.error("Specify that you are on the " + platform + " platform,");
  console.error("and on the " + arch + " architecture.");
  process.exit(1);
}

copyPlatformBinaries(platformPath);
