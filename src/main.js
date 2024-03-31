// entrypoint of the program; this is the cli

import { deploy } from "./deploy";
import { sourceDir } from "./constants";
import { cli } from "utils/cli";
import { buildFromPath } from "./build.js";
import { singleFileServer, directoryServer } from "./server";
import { Path } from "utils/path";
import {
  sourceDir as SITE_DIRECTORY,
  siteName,
  deploymentBranch,
  localPort,
} from "./constants";

const localhostUrl = `http://localhost`;
const wsLocalhostUrl = `ws://localhost`;
const devWebsocketPath = "/__devsocket";

// paths to ignore by default from the website we build
const commonIgnorePaths = [".git", "node_modules"];

/**
 * Build a website from the incoming paths.
 */
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
    siteName,
    rootUrl,
    sourceDir: sourceDir,
    fallbackSourceDir,
    targetDir: targetDir,
    // 'ignorePaths' are expected to be absolute
    ignorePaths: commonIgnorePaths.map((p) => sourceDir.toString() + "/" + p),
  });
};

/**
 * Deploy the current website.
 */
const deployWebsite = () => {
  const currentRepo = Path.create(".").repo;

  deploy({
    currentRepo,
    deploymentBranch,
    targetDir: Path.create("./docs").toString(),
  });
};

/**
 * Serve whatever is on the path provided.
 * @param {*} incomingPaths a list of paths to serve from.
 */
const serve = (incomingPaths) => {
  const paths = incomingPaths?.length ? incomingPaths : ["."];

  const path = Path.create(paths[0]);

  // if we were provided a dir, that directory
  // becomes the root of a tree we serve
  if (path.isDirectory()) {
    directoryServer({
      absolutePathToDirectory: path,
      fallbackDirPath: sourceDir,
      url: localhostUrl,
      localPort,
    });
  }

  // otherwise, we serve just the file that was pointed to from all paths
  // this is mostly useless because html files can't pull in resources, for ex.,
  // but it's good for testing the parsing and interpretation of new file types.
  else {
    singleFileServer({
      url: localhostUrl,
      localPort,
      absolutePathToFile: path,
      siteName: "Jake Chvatal",
    });
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
