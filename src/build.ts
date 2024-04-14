import { readFile } from "./file";
import type { PageSettings } from "./types/site";
import { File } from "./file/classes";

/**
 * Given a relative path, build the whole file tree as static html.
 */
const buildSiteFromFile = (
  file: File,
  settings: PageSettings,
  filesSeenSoFar: Set<string>
) => {
  const dependencyPath = file.path.toString();

  // if we've already seen this file, we don't need to build it again.
  if (filesSeenSoFar.has(dependencyPath)) {
    return;
  }

  // register that we've seen the file so we don't build it again.
  filesSeenSoFar.add(dependencyPath);

  // write the file to disk. (note: may need more context.)
  file.write(settings);

  const dependencies = file.dependencies(settings);

  dependencies
    .filter((f) => !filesSeenSoFar.has(f.path.toString()))
    .map((dependencyFile) => {
      buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
    });
};

// build a website from a path to a directory.
// requires:
// { siteName, rootUrl, sourceDir, targetDir }
const buildFromPath = (settings: PageSettings) => {
  const { sourceDir, targetDir, ignorePaths } = settings;

  // start off from the root, source dir,
  // bootstrap by reading the root file as HTML
  const dir = readFile(sourceDir.toString() + "/index.html", settings);

  console.log("Starting with", dir.path.toString());

  // if we've already seen a file path, we should ignore it.
  // ignore paths the user is provided and the target dir --
  // the target dir could be a subdirectory of the source dir,
  // and we don't want to build the site into itself.
  const filePathsSeenSoFar = new Set([
    ...(ignorePaths ?? []),
    ...(ignorePaths ?? []).map((p) => p + ".html"),
    targetDir.toString(),
    targetDir.toString() + ".html",
    targetDir.toString() + "/index.html",
  ]);

  buildSiteFromFile(dir, settings, filePathsSeenSoFar);
};

export { buildFromPath };
