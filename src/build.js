import { readFile } from "./file";

// PROGRESS:
// - some files render as plaintext, not html first, when serving.
// - it's unclear why - not able to easily find a pattern.
// the folders with '.html' appended are weird.
// - figure out why serve fn behaves a bit strangely. there must be some stupid http way

// given a relative path, build the whole file tree as static html.
const buildSiteFromFile = (file, settings, filesSeenSoFar) => {
  const { siteName, rootUrl, sourceDir, targetDir } = settings;

  const dependencyPath = file.path.toString();

  // if we've already seen this file, we don't need to build it again.
  if (filesSeenSoFar.has(dependencyPath)) {
    return;
  }

  // register that we've seen the file so we don't build it again.
  filesSeenSoFar.add(dependencyPath);

  // write the file to disk. (note: may need more context.)
  file.write(settings);

  const dependencies = file.dependencies(settings, filesSeenSoFar);
  console.log('Building a file!', dependencyPath);
  console.log("Dependencies:", dependencies.map((d) => d.path.toString()));

  // Write all of the dependencies of the file (that we haven't seen yet) to disk.
  dependencies.filter(f => !filesSeenSoFar.has(f.path.toString())).forEach((dependencyFile) => {
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
  const filePathsSeenSoFar = new Set([
    ...ignorePaths,
    ...ignorePaths.map(p => p + '.html'),
     targetDir,
     targetDir + '.html',
     targetDir + '/index.html',
    ]);

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { buildFromPath };
