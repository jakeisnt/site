// entrypoint of the program; this is the cli

import { deploy as siteDeploy } from "./deploy";
import { cli } from "utils/cli";
import { buildFromPath } from "./build.js";
import { singleFileServer, directoryServer } from "./server";
import { Path } from "utils/path";

const makeConfig = () => {
  const siteName = "Jake Chvatal";
  const host = `http://localhost`;
  const url = `http://${host}`;
  const port = 4242;
  const websocketPath = "/__devsocket";
  const sourceDir = Path.create("./");
  const targetDir = sourceDir; // sourceDir.join("/docs");
  const fallbackSourceDir = sourceDir;
  const rootUrl = `http://${url}:${port}`;
  const resourcesDir = Path.create(sourceDir.toString() + "/resources");
  const faviconsDir = Path.create(sourceDir.toString() + "/favicons");

  // paths to ignore by default from the website we build
  const ignorePaths = [".git", "node_modules"].map(
    (p) => sourceDir.toString() + "/" + p
  );

  return {
    siteName,
    sourceDir,
    targetDir,
    fallbackSourceDir,
    fallbackDirPath: fallbackSourceDir,

    url,
    host,
    port,
    rootUrl,

    resourcesDir,
    faviconsDir,
    ignorePaths,
    websocketPath,
  };
};

const cfg = makeConfig();

/**
 * Build a website from the incoming paths.
 * Usage: `site build`
 */
const build = () => buildFromPath(cfg);

/**
 * Deploy the current website.
 */
const deploy = () => {
  const currentRepo = Path.create(".").repo;

  if (!currentRepo) {
    console.log(
      "Current path does not have a git repository. Unable to deploy."
    );
    return;
  }

  siteDeploy({
    currentRepo,
    deploymentBranch: "production",
    targetDir: cfg.targetDir.toString(),
  });
};

/**
 * Serve whatever is on the path provided.
 * @param {*} incomingPaths a list of paths to serve from.
 */
const serve = (incomingPaths?: string[]) => {
  // TODO: Framework should handle type-based argument casting
  //   and convert to names when possible.
  const paths = incomingPaths?.length ? incomingPaths : ["."];
  const path = Path.create(paths[0]);

  // If we were provided a directory,
  // serve the directoriy and its contents recursively.
  if (path.isDirectory({ noFSOperation: true })) {
    directoryServer(cfg);
  }

  // Otherwise, we serve the file that was pointed to from all paths.
  else {
    singleFileServer({ ...cfg, absolutePathToFile: path });
  }
};

const app = cli("site")
  .describe("compiles the website")
  .option("deploy")
  .describe("deploy the website")
  .action(deploy)
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
