// entrypoint of the program; this is the cli

import { deploy } from "./deploy";
import { sourceDir } from "./constants";
import { cli } from "utils/cli";
import { buildFromPath } from "./build.js";
import { singleFileServer, directoryServer } from "./server";
import { Path } from "utils/path";
import { sourceDir as SITE_DIRECTORY } from "./constants";

// paths to ignore by default from the website we build
const commonIgnorePaths = [".git", "node_modules"];

const build = (incomingPaths) => {
  const paths = incomingPaths?.length ? incomingPaths : ["."];

  if (!paths[0]) {
    throw new Error(`No source directory provided. Exiting!`);
  }

  const sourceDir = Path.create(paths[0]);

  const targetDir = paths[1]
    ? Path.create(paths[1])
    : Path.create(sourceDir.toString() + "/docs");

  const fallbackSourceDir = paths[2]
    ? Path.create(paths[2])
    : Path.create(SITE_DIRECTORY);

  const rootUrl = paths[3] ?? "file://" + targetDir.toString();

  buildFromPath({
    siteName: "Jake Chvatal",
    rootUrl,
    sourceDir: sourceDir,
    fallbackSourceDir,
    targetDir: targetDir,
    // 'ignorePaths' are expected to be absolute
    ignorePaths: commonIgnorePaths.map((p) => sourceDir.toString() + "/" + p),
  });
};

const deployWebsite = () => {
  const currentRepo = Path.create(".").repo;

  deploy({
    currentRepo,
    deploymentBranch: "production",
    targetDir: Path.create("./docs").toString(),
  });
};

// Serve whatever's at the first path
const serve = (incomingPaths) => {
  const paths = incomingPaths?.length ? incomingPaths : ["."];

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
  .action(deployWebsite)
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
