// entrypoint of the program; this is the cli

import { deploy } from "./deploy";
import { deploymentBranch, sourceDir, targetDir, website } from "./constants";
import { cli } from "utils/cli";
import { buildFromPath } from "./build.js";
import { singleFileServer, directoryServer } from "./server";
import { Path } from "utils/path";

// paths to ignore by default from the website we build
const commonIgnorePaths = [".git", "node_modules"];

const build = (paths) => {
  if (!paths.length) {
    console.log("No file path specified. Not building anything.");
    return;
  }

  const sourceDir = Path.create(paths[0]);
  const targetDir = paths[1]
    ? Path.create(paths[1])
    : Path.create(sourceDir.toString() + "/docs");

  const rootUrl = paths[2] ?? "file://" + targetDir.toString();

  buildFromPath({
    siteName: "Jake Chvatal",
    rootUrl,
    sourceDir: sourceDir,
    targetDir: targetDir,
    // 'ignorePaths' are expected to be absolute
    ignorePaths: commonIgnorePaths.map((p) => sourceDir.toString() + "/" + p),
  });
};

// Serve whatever's at the first path
const serve = (paths) => {
  if (!paths.length) {
    console.log("No file path specified. Not serving anything.");
    return;
  }

  const path = Path.create(paths[0]);

  // if we were provided a dir, that directory
  // becomes the root of a tree we serve
  if (path.isDirectory()) {
    directoryServer(path, sourceDir);
  }

  // otherwise, we serve just the file that was pointed to from all paths
  // this is mostly useless because html files can't pull in resources, for ex.,
  // but it's good for testing the parsing and interpretation of new file types.
  else {
    singleFileServer(path);
  }
};

const app = cli("site")
  .describe("compiles the website")
  .option("deploy")
  .describe("deploy the website")
  .action(() => console.log("deploy"))
  .option("build")
  .describe("build the website")
  .action(build)
  .option("serve")
  .describe("serve the website")
  .action(serve);

function main() {
  const args = process.argv.slice(2);
  app.exec(args);
}

main();
