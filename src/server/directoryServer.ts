import { Path } from "../utils/path";

import log from "utils/log";

import { formatUrl, makeFileResponse } from "./utils";
import { readFile } from "../file";
import { createServer } from "./createServer";
import Directory from "../file/filetype/directory";
import { homePage } from "../pages/home";

/**
 * Serve the files in a directory.
 * @param {*} absolutePathToDirectory primary directory we should source files from.
 * @param {*} fallbackDirPath path to directory we should fall back to serving.
 */
const directoryServer = ({
  absolutePathToDirectory,
  fallbackDirPath,
  url,
  port,
  siteName,
  devWebsocketUrl,
}: {
  absolutePathToDirectory: Path;
  fallbackDirPath: string;
  url: string;
  port: number;
  siteName: string;
  devWebsocketUrl: string;
}) => {
  const dir = readFile(absolutePathToDirectory, {
    sourceDir: absolutePathToDirectory.toString(),
    fallbackSourceDir: fallbackDirPath,
  }) as unknown as Directory;

  const devUrl = formatUrl({ url, port });
  let fallbackDir: Directory;

  try {
    if (fallbackDirPath) {
      fallbackDir = readFile(fallbackDirPath, {
        sourceDir: absolutePathToDirectory.toString(),
        fallbackSourceDir: fallbackDirPath,
      }) as unknown as Directory;
    }
  } catch (e: any) {
    log.debug("Error finding fallback dir:", e.message);
  }

  if (!dir.isDirectory()) {
    throw new Error(
      `Received path '${absolutePathToDirectory}' is not a directory`
    );
  }

  const sourceDir = dir.path.toString();
  const resourcesDir = `${sourceDir}/resources`;
  const faviconsDir = `${sourceDir}/favicons`;

  createServer({
    url,
    port,
    websocketPath: devWebsocketUrl,
    onRequest: ({ path }: { path: Path }) => {
      let pathToUse = Path.create(path);

      console.log(pathToUse.toString());
      // If we request the root, serve up the home page
      if (["", "/", "/index", "/index.html"].includes(pathToUse.toString())) {
        return makeFileResponse(homePage(), {
          sourceDir: dir.path.toString(),
          siteName,
          devUrl,
          devWebsocketUrl,
          resourcesDir,
          faviconsDir,
          targetDir: sourceDir,
        });
      }

      if (path.name === "index.html") {
        // if the path is a directory, serve the parent like an html file
        pathToUse = Path.create(path.parent.toString() + ".html");
      }

      log.debug("Finding file: ", dir.toString(), pathToUse.toString());
      let file = dir.findFile(pathToUse);
      if (!file) {
        // if we can't find the file, attempt to find it in a fallback directory.
        file = fallbackDir?.findFile(pathToUse);
      }

      if (!file) {
        return new Response("Not found", { status: 404 });
      }

      return makeFileResponse(file, {
        siteName,
        sourceDir,
        devUrl,
        devWebsocketUrl,
        resourcesDir,
        faviconsDir,
        targetDir: sourceDir,
      });
    },

    onSocketConnected: (ws) => {
      log.hotReload("hot reload socket connected");
    },
  });
};

export { directoryServer };
