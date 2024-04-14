import { readFile } from "./file";
import type { PageSettings } from "./types/site";
import { File } from "./file/classes";
import { homePage } from "./pages/home";

/**
 * Recursively build a website, starting with
 * the provided file and building all of its dependencies.
 */
const buildSiteFromFile = (
  file: File,
  settings: PageSettings,
  filesSeenSoFar: Set<string>
) => {
  if (filesSeenSoFar.has(file.path.toString())) return;
  filesSeenSoFar.add(file.path.toString());

  file.write(settings);

  file.dependencies(settings).map((dependencyFile) => {
    buildSiteFromFile(dependencyFile, settings, filesSeenSoFar);
  });
};

/**
 * Build a site from a configuration.
 */
const buildFromPath = (settings: PageSettings) => {
  const { sourceDir, targetDir, ignorePaths } = settings;

  // Write the root file
  const rootFile = targetDir.join("/index.html");
  rootFile.writeString(homePage(settings).serve(settings).contents);

  // Read the rest of the repo under `source`.
  const cfg = { ...settings, targetDir: targetDir.join("/source") };

  // Start off from the root, source dir,
  // Bootstrap the process by reading the root file as HTML.
  const dir = readFile(sourceDir.join("/index.html"), cfg);
  console.log("Starting with", dir.path.toString());

  // If we've already seen a file path, we should ignore it.
  // Ignore paths the user is provided and the target dir --
  // the target dir could be a subdirectory of the source dir
  // and we don't want to build the site into itself.
  const filePathsSeenSoFar = new Set([
    ...(ignorePaths ?? []),
    ...(ignorePaths ?? []).map((p) => p + ".html"),
    targetDir.toString(),
    // hardcode in the .git ignore path so i dont fuck up
    sourceDir + "/.git",
    sourceDir + "/.direnv",
    sourceDir + "/node_modules",
    targetDir.toString() + ".html",
    targetDir.toString() + "/index.html",
  ]);

  buildSiteFromFile(dir, cfg, filePathsSeenSoFar);
};

export { buildFromPath };
