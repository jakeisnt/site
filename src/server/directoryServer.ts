import { Path } from "../utils/path";

import log from "utils/log";

import { formatUrl, makeFileResponse } from "./utils";
import { readFile } from "../file";
import { makeHomePage } from "pages/home";
import { createServer } from "./createServer";
import Directory from "../file/filetype/directory";

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
  });

  const devUrl = formatUrl({ url, port });
  let fallbackDir: Directory;

  try {
    fallbackDir =
      fallbackDirPath &&
      readFile(fallbackDirPath, {
        sourceDir: absolutePathToDirectory.toString(),
        fallbackSourceDir: fallbackDirPath,
      });
  } catch (e) {
    log.debug("Error finding fallback dir:", e.message);
  }

  if (!dir.isDirectory()) {
    throw new Error(
      `Received path '${absolutePathToDirectory}' is not a directory`
    );
  }

  createServer({
    url,
    port,
    websocketPath: devWebsocketUrl,
    onRequest: ({ path }: { path: Path }) => {
      let pathToUse = Path.create(path);

      // if we request the root, serve up the home page
      // TODO this seems to just not work.
      //   if (["/", "/index", "/index.html"].includes(pathToUse.toString())) {
      //     return makeFileResponse(
      //       { serve: makeHomePage },
      //       {
      //         sourceDir: dir.path,
      //         siteName,
      //         devUrl,
      //         devWebsocketUrl,
      //       }
      //     );
      //   }

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
        sourceDir: dir.path,
        devUrl,
        devWebsocketUrl,
      });
    },

    onSocketConnected: (ws) => {
      log.hotReload("hot reload socket connected");
    },
  });
};

export { directoryServer };
