const build = (websiteSpec) => {};

// given a relative path, build the whole file tree as static html.
const buildSiteFromFile = (file, settings, filesSeenSoFar) => {
  const { siteName, rootUrl, sourceDir, targetDir } = settings;

  // register that we've seen the file.
  filesSeenSoFar.push(dependencyPath);

  // if the file is html,
  if (file.isHtml()) {
    // Get the file as HTML.
    const htmlExport = file.asHtml({ siteName, rootUrl, sourceDir });

    // Write the file as HTML to disk.

    // Write all of the dependencies of the file (that we haven't seen yet) to disk.
    htmlExport.dependencies.forEach((dependencyFile) => {
      if (!filesSeenSoFar.contains(dependencyFile.path.toString())) {
        buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
      }
    });
  } else {
    // if the file isn't html, it doesn't have dependencies (yet).
    // write the file as it is to disk.
    file.write();
  }
};

const buildFromPath = (settings) => {
  const dir = readFile(sourceDir);

  // if we've already seen a file path, we should ignore it
  const filePathsSeenSoFar = new Set();

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { build };
