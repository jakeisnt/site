import { Path } from '../utils/path';
import { link } from '../utils/printstyle';

import { readFile } from '../file';
import { makeHomePage } from 'pages/home';

const localPort = 4242; // Your desired port
const localhostUrl = `http://localhost`;
const wsLocalhostUrl = `ws://localhost`;

const devWebsocketPath = '/__devsocket';

// format url
const formatUrl = ({ url, port, path }) => `${url}${port ? ':' + port : ''}${path ?? ''}`;

const devUrl = formatUrl({ url: localhostUrl, port: localPort });
const httpWebsocketUrl = formatUrl({ url: localhostUrl, port: localPort, path: devWebsocketPath });
const devWebsocketUrl = formatUrl({ url: wsLocalhostUrl, port: localPort, path: devWebsocketPath });


// get the goods without the url
const withoutUrl = (fullPath, url) => fullPath.replace(url, '');

// is the file html?
const isHtml = (file) => file.mimeType === 'text/html';

// get the html version of a file
const getNonHtmlPath = (file) => {
  const htmlPath = file.path + '.html';
  const htmlFile = readFile(htmlPath);
  return htmlFile;
};

// inject a hot reload script into the body iof an html string
const injectHotReload = (htmlString) => {
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

  return htmlString.replace('</body>', `${script}</body>`);
}

// make a response to a request for a file with the file
const fileResponse = (file, { sourceDir }) => {
  const toServe = file.serve({
    siteName: 'Jake Chvatal',
    rootUrl: devUrl,
    sourceDir,
  })

  let response = isHtml(file) ? injectHotReload(toServe) : toServe;

  return new Response(
    response,
    {
      headers: {
        'content-type': file?.mimeType ?? 'text/html',
      }
    });
};

// Start a server at the provided URL and port.
// Handle requests with the provided callback.
const createServer = (
  { url = localhostUrl, port = localPort, onRequest = () => {}, onSocketConnected = () => {} } = {}
) => {
  const fullUrl = formatUrl({ url, port });
  const linkText = link(fullUrl).underline().color('blue');

  console.log(`Starting server at ${linkText}`);

  const server = Bun.serve({
    port,
    fetch(req, server) {
      const path = withoutUrl(req.url, fullUrl);
      console.log('FETCH', path);
      if (req.url === httpWebsocketUrl) {
        console.log('websocket request');
        if (server.upgrade(req)) {
          return;
        }
      }

      return onRequest({ req, server, path: Path.create(path) });
    },
    websocket: {
      open(ws) {
        onSocketConnected(ws);
      }
    }
  });

  return server;
}

// a hot-reloading server for a single file
const singleFileServer = (absolutePathToFile) => {
  const file = readFile(absolutePathToFile);

  let wsClientConnection = null;

  createServer({
    onRequest: ({ request }) => {
      return fileResponse(file, { sourceDir: file.path });
    },
    onSocketConnected: (ws) => {
      console.log('socket connected');
      wsClientConnection = ws;
    }
  });

  file.watch((eventType, curFile) => {
    if (eventType === 'change') {
      console.log('File changed. Reloading...');
      // re-read the file into memory
      file.read();
      wsClientConnection?.send('reload');
    }
  });
}

// support serving arbitrary files from a directory;
// this means we have to handle routing.
const directoryServer = (absolutePathToDirectory) => {
  const dir = readFile(absolutePathToDirectory);

  if (!dir.isDirectory) {
    throw new Error(`Received path '${absolutePathToDirectory}' is not a directory`);
  }

  createServer({
    onRequest: ({ path }) => {
      let pathToUse = Path.create(path);

      // if we request the root, serve up the home page
      // TODO this needs a more elegant solution
      if (['/', '/index', '/index.html'].includes(pathToUse)) {
        return fileResponse({ serve: makeHomePage }, { sourceDir: dir.path });
      }

      if (path.name === 'index.html') {
        // if the path is a directory, serve the parent like an html file
        pathToUse = Path.create(path.parent.pathString + '.html');
      }

      const file = dir.findFile(pathToUse);

      if (!file) {
        return new Response('Not found', { status: 404 });
      }

      return fileResponse(file, { sourceDir: dir.path });
    },

    onSocketConnected: (ws) => {
      console.log('socket connected');
    }
  });
}

export { singleFileServer, directoryServer };
