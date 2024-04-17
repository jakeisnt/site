import { Path } from "../utils/path";

import type { PageSettings } from "../types/site";
import log from "utils/log";

import { makeFileResponse } from "./utils";
import { readFile } from "../file";
import { createServer } from "./createServer";
import Directory from "../file/filetype/directory";
import { homePage } from "../pages/home";

/**
 * Serve the files in a directory.
 * @param {*} absolutePathToDirectory primary directory we should source files from.
 * @param {*} fallbackDirPath path to directory we should fall back to serving.
 */
const directoryServer = (settings: PageSettings) => {
  let pageSettings = settings;

  const dir = readFile(
    settings.sourceDir,
    pageSettings
  ) as unknown as Directory;
  let fallbackDir: Directory;

  try {
    if (settings.fallbackSourceDir) {
      fallbackDir = readFile(
        settings.fallbackSourceDir,
        pageSettings
      ) as unknown as Directory;
    }
  } catch (e: any) {
    log.debug("Error finding fallback dir:", e.message);
  }

  if (!dir.isDirectory()) {
    throw new Error(`Received path '${settings.sourceDir}' is not a directory`);
  }

  pageSettings = { ...settings, sourceDir: dir.path };

  createServer({
    ...settings,
    onRequest: ({ path }: { path: Path }) => {
      let pathToUse = path;

      // If we request the root, serve up the home page. Hardcoded.
      if (["", "/", "/index", "/index.html"].includes(pathToUse.toString())) {
        return makeFileResponse(homePage(pageSettings), pageSettings);
      }

      // Otherwise, we get a bit fancy.
      // Replace the target dir with the source dir where we're looking.
      // This allows the target dir to be an arbitrary subdomain of the source,
      // and allows us to patch back to the source path from the target.
      pathToUse = Path.create(
        path.toString().startsWith("/") ? path.toString().slice(1) : path
      ).relativeTo(pageSettings.targetDir, pageSettings.sourceDir);

      let file = dir.findFile(pathToUse, pageSettings);
      if (!file) {
        // if we can't find the file, attempt to find it in a fallback directory.
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
