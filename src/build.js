const build = (websiteSpec) => {};

// given a relative path, build the whole file tree as static html.
const buildSiteFromFile = (file, settings, filesSeenSoFar) => {
  const { siteName, rootUrl, sourceDir, targetDir } = settings;

  // register that we've seen the file.
  filesSeenSoFar.push(dependencyPath);

  // write the file to disk. (note: needs more context for sure.)
  file.write(settings);

  // Write all of the dependencies of the file (that we haven't seen yet) to disk.
  file.dependencies(settings).forEach((dependencyFile) => {
    if (!filesSeenSoFar.contains(dependencyFile.path.toString())) {
      buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
    }
  });
};

const buildFromPath = (settings) => {
  const dir = readFile(sourceDir);

  // if we've already seen a file path, we should ignore it
  const filePathsSeenSoFar = new Set();

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { build };
