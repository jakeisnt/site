import { Path } from "../utils/path";
import { link } from "../utils/printstyle";

import log from "utils/log";

import { readFile } from "../file";
import { makeHomePage } from "pages/home";

/**
 * Format a URL with the URL, port, and path.
 */
const formatUrl = ({ url, port, path }) =>
  `${url}${port ? ":" + port : ""}${path ?? ""}`;

/**
 * Remove the full path from the URL.
 */
const withoutUrl = (fullPath, url) => fullPath.replace(url, "");

/**
 * inject a hot reload script into the body iof an html string.
 * @param {*} param0
 * @returns
 */
const injectHotReload = ({ htmlString, devWebsocketUrl }) => {
  const wsUrl = devWebsocketUrl;
  const script = `
    <script>
      console.log('hot reload script loaded');
      const socket = new WebSocket('${wsUrl}');
      socket.addEventListener('message', function (event) {
        console.log('Received message from server ', event.data);
        window.location.reload();
      });
    </script>
  `;

  return htmlString.replace("</body>", `${script}</body>`);
};

/**
 * make a response to a request for a file with the file
 * @param {*} file
 */
const makeFileResponse = (
  file,
  { siteName, sourceDir, devUrl, devWebsocketUrl }
) => {
  const { contents, mimeType } = file.serve({
    siteName,
    rootUrl: devUrl,
    sourceDir,
  });

  let responseText =
    mimeType === "text/html"
      ? injectHotReload({ htmlString: contents, devWebsocketUrl })
      : contents;

  return new Response(responseText, {
    headers: {
      // NEVER cache. this is always a dev server.
      "Cache-Control": "no-store, no-cache, must-revalidate, proxy-revalidate",
      Pragma: "no-cache",
      Expires: "0",
      // content-type (required)
      "content-type": mimeType,
    },
  });
};

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
} = {}) => {
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

      return onRequest({ req, server, path: Path.create(path) });
    },
    websocket: {
      open(ws) {
        onSocketConnected(ws);
      },
    },
  });

  return server;
};

/**
 * A hot-reloading single file server.
 */
const singleFileServer = ({ url, localPort, absolutePathToFile, siteName }) => {
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
    onRequest: ({ request }) => {
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

/**
 * Serve the files in a directory.
 * @param {*} absolutePathToDirectory primary directory we should source files from.
 * @param {*} fallbackDirPath path to directory we should fall back to serving.
 */
const directoryServer = ({
  absolutePathToDirectory,
  fallbackDirPath,
  url,
  localPort,
  siteName,
  devWebsocketUrl,
}) => {
  const dir = readFile(absolutePathToDirectory);
  const devUrl = formatUrl({ url, port: localPort });
  let fallbackDir;

  try {
    fallbackDir = fallbackDirPath && readFile(fallbackDirPath);
  } catch (e) {
    log.debug("Error finding fallback dir:", e.message);
  }

  if (!dir.isDirectory) {
    throw new Error(
      `Received path '${absolutePathToDirectory}' is not a directory`
    );
  }

  createServer({
    url,
    port: localPort,
    onRequest: ({ path }) => {
      let pathToUse = Path.create(path);

      // if we request the root, serve up the home page
      // TODO this needs a more elegant solution
      if (["/", "/index", "/index.html"].includes(pathToUse)) {
        return makeFileResponse(
          { serve: makeHomePage },
          {
            sourceDir: dir.path,
            siteName,
            devUrl,
            devWebsocketUrl,
          }
        );
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

      return makeFileResponse(file, { sourceDir: dir.path });
    },

    onSocketConnected: (ws) => {
      log.hotReload("hot reload socket connected");
    },
  });
};

export { singleFileServer, directoryServer };
