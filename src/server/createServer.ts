import { Path } from "../utils/path";
import { link } from "../utils/printstyle";
import { formatUrl, withoutUrl } from "./utils";
import log from "../utils/log";

type OnRequestType = (cfg: { path: Path }) => any;
type OnSocketConnectedType = (ws: any) => any;

/**
 * Start a server at the provided URL and port.
 * Handle requests with the provided callback.
 */
const createServer = ({
  url,
  port,
  websocketPath,
  onRequest = () => {},
  onSocketConnected = () => {},
}: {
  url: string;
  port: number;
  websocketPath: string;
  onRequest: OnRequestType;
  onSocketConnected: OnSocketConnectedType;
}) => {
  const fullUrl = formatUrl({ url, port });
  const linkText = link(fullUrl).underline().color("blue");

  const httpWebsocketUrl = formatUrl({
    url,
    port,
    path: websocketPath,
  });

  log.production(`Starting server at ${linkText}`);

  const server = Bun.serve({
    port,
    fetch(req, server) {
      const path = withoutUrl(req.url, fullUrl);
      log.network("FETCH", path);
      if (req.url === httpWebsocketUrl) {
        log.network("websocket request");
        if (server.upgrade(req)) {
          return;
        }
      }

      return onRequest({
        // req,
        // server,
        path: Path.create(path),
      });
    },
    websocket: {
      // TODO: what does this field do?
      message(ws, message) {
        log.network("Received message from client: ", message);
      },
      open(ws) {
        onSocketConnected(ws);
      },
    },
  });

  return server;
};

export { createServer };
