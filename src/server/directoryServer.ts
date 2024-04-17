import { Path } from "../utils/path";

import type { PageSettings } from "../types/site";
import log from "utils/log";

import { makeFileResponse } from "./utils";
import { readFile } from "../file";
import { createServer } from "./createServer";
import Directory from "../file/filetype/directory";
import { homePage } from "../pages/home";

/**
 * Load the specified source files from disk,
 * bootstrapping the source finding process.
 */
const findSource = (
  settings: PageSettings
): {
  fallbackDir?: Directory;
  dir: Directory;
} => {
  const dir = readFile(settings.sourceDir, settings) as unknown as Directory;
  let fallbackDir: Directory | undefined;

  try {
    if (settings.fallbackSourceDir) {
      fallbackDir = readFile(
        settings.fallbackSourceDir,
        settings
      ) as unknown as Directory;
    }
  } catch (e: any) {
    log.debug("Error finding fallback dir:", e.message);
  }

  if (!dir.isDirectory()) {
    throw new Error(`Received path '${settings.sourceDir}' is not a directory`);
  }

  return { dir, fallbackDir };
};

/**
 * Serve the files in a directory.
 */
const directoryServer = (settings: PageSettings) => {
  let pageSettings = settings;
  const { dir, fallbackDir } = findSource(settings);
  pageSettings = { ...settings, sourceDir: dir.path };

  createServer({
    ...settings,
    onRequest: ({ path }: { path: Path }) => {
      let pathToUse = path;

      // If we request the root, serve up the home page. Hardcoded.
      if (["", "/", "/index", "/index.html"].includes(pathToUse.toString())) {
        return makeFileResponse(homePage(pageSettings), pageSettings);
      }

      // Otherwise:
      // - Replace the target dir with the source dir where we're looking.
      //   Allows the target dir to be an arbitrary subdomain of the source,
      //   and to to patch back to the source path from the target.
      pathToUse = Path.create(
        path.toString().startsWith("/") ? path.toString().slice(1) : path
      ).relativeTo(pageSettings.targetDir, pageSettings.sourceDir);

      let file = dir.findFile(pathToUse, pageSettings);
      if (!file) {
        // If we can't find the file, attempt to find it in a fallback directory.
        file = fallbackDir?.findFile(pathToUse, pageSettings);
      }

      if (!file) {
        return new Response("Not found", { status: 404 });
      }

      return makeFileResponse(file, pageSettings);
    },

    onSocketConnected: (ws: WebSocket) => {
      log.hotReload("hot reload socket connected");
    },
  });
};

export { directoryServer };
