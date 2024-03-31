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
}) => {
  const file = readFile(absolutePathToFile);
  const devUrl = formatUrl({ url, port: localPort });

  const devWebsocketUrl = formatUrl({
    url: wsLocalhostUrl,
    port: localPort,
    path: devWebsocketPath,
  });

  let wsClientConnection = null;

  createServer({
    url,
    port: localPort,
    websocketPath: devWebsocketPath,
    onRequest: () => {
      return makeFileResponse(file, {
        siteName,
        sourceDir: file.path,
        devUrl,
        devWebsocketUrl,
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
