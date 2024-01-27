import { readFile } from "./file";

// given a relative path, build the whole file tree as static html.
const buildSiteFromFile = (file, settings, filesSeenSoFar) => {
  const { siteName, rootUrl, sourceDir, targetDir } = settings;

  const dependencyPath = file.path.toString();

  // if we've already seen this file, we don't need to build it again.
  if (filesSeenSoFar.has(dependencyPath)) {
    console.log("Already built file at path ", dependencyPath);
    return;
  }

  if (file.isDirectory) {
    console.log("Building directory at path ", dependencyPath);
  } else {
    console.log("Building file at path ", dependencyPath);
  }

  // register that we've seen the file.
  filesSeenSoFar.add(dependencyPath);

  // write the file to disk. (note: needs more context for sure.)
  file.write(settings);

  // Write all of the dependencies of the file (that we haven't seen yet) to disk.
  file.dependencies(settings).forEach((dependencyFile) => {
    buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
  });
};

// build a website from a path to a directory.
// requires:
// { siteName, rootUrl, sourceDir, targetDir }
const buildFromPath = (settings) => {
  const { sourceDir, targetDir, ignorePaths } = settings;

  // start off from the root, source dir
  const dir = readFile(sourceDir);

  console.log("Starting with", dir.path.toString());

  // if we've already seen a file path, we should ignore it.
  // ignore paths the user is provided and the target dir --
  // the target dir could be a subdirectory of the source dir,
  // and we don't want to build the site into itself.
  const filePathsSeenSoFar = new Set([...ignorePaths, targetDir]);

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { buildFromPath };
