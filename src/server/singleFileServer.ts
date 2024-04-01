import { formatUrl, makeFileResponse } from "./utils";
import { createServer } from "./createServer";
import { readFile } from "../file";
import log from "utils/log";

/**
 * A hot-reloading single file server.
 */
const singleFileServer = ({
  url,
  localPort,
  absolutePathToFile,
  siteName,
  wsLocalhostUrl,
  devWebsocketPath,
}: {
  url: string;
  localPort: number;
  absolutePathToFile: string;
  siteName: string;
  wsLocalhostUrl: string;
  devWebsocketPath: string;
}) => {
  const file = readFile(absolutePathToFile);
  const devUrl = formatUrl({ url, port: localPort });

  const devWebsocketUrl = formatUrl({
    url: wsLocalhostUrl,
    port: localPort,
    path: devWebsocketPath,
  });

  // TODO: hunt down the websocket type and use it properly
  let wsClientConnection: any = null;

  const sourceDir = file.path.parent.toString();
  const resourcesDir = `${sourceDir}/resources`;
  const faviconsDir = `${sourceDir}/favicons`;

  createServer({
    url,
    port: localPort,
    websocketPath: devWebsocketPath,
    onRequest: () => {
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
      log.network("socket connected");
      wsClientConnection = ws;
    },
  });

  file.watch((eventType, curFile) => {
    if (eventType === "change") {
      log.hotReload("File changed. Reloading...");
      // re-read the file into memory
      file.read();
      wsClientConnection?.send("reload");
    }
  });
};

export { singleFileServer };
