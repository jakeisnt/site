import { readFile } from "./file";

// given a relative path, build the whole file tree as static html.
const buildSiteFromFile = (file, settings, filesSeenSoFar) => {
  const { siteName, rootUrl, sourceDir, targetDir } = settings;

  const dependencyPath = file.path.toString();

  console.log("building file", dependencyPath);

  // register that we've seen the file.
  filesSeenSoFar.add(dependencyPath);

  // write the file to disk. (note: needs more context for sure.)
  file.write(settings);

  // Write all of the dependencies of the file (that we haven't seen yet) to disk.
  file.dependencies(settings, filesSeenSoFar).forEach((dependencyFile) => {
    buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
  });
};

// build a website from a path to a directory.
// requires:
// { siteName, rootUrl, sourceDir, targetDir }
const buildFromPath = (settings) => {
  const { sourceDir } = settings;

  // start off from the root, source dir
  const dir = readFile(sourceDir);

  console.log("starting with", dir.path.toString());

  // if we've already seen a file path, we should ignore it
  const filePathsSeenSoFar = new Set();

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { buildFromPath };
