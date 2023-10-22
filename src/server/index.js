import { Path } from '../utils/path';
import { link } from '../utils/printstyle';

import { readFile } from '../file';

const constPort = 4242; // Your desired port
const targetDir = '/your/target/directory'; // Replace with your target directory
const localhostUrl = `http://localhost`;
const wsLocalhostUrl = `ws://localhost`;
const sourceDir = '/home/jake/site';

const devWebsocketPath = '/__devsocket';

const httpWebsocketUrl = `${localhostUrl}:${constPort}${devWebsocketPath}`;
const devWebsocketUrl = `${wsLocalhostUrl}:${constPort}${devWebsocketPath}`;

// format url
const formatUrl = ({ url, port }) => `${url}${port ? ':' + port : ''}/`;

// Start a server at the provided URL and port.
// Handle requests with the provided callback.
const createServer = (
  { url = localhostUrl, port = constPort, onRequest = () => {}, onSocketConnected = () => {} } = {}
) => {
  const fullUrl = formatUrl({ url, port });
  const linkText = link(fullUrl).underline().color('blue');

  console.log(`Starting server at ${linkText}`);

  const server = Bun.serve({
    port,
    fetch(req, server) {
      console.log('FETCH', req.url);
      if (req.url === httpWebsocketUrl) {
        console.log('websocket request');
        if (server.upgrade(req)) {
          return;
        }
      }

      return onRequest(req, server);
    },
    websocket: {
      open(ws) {
        onSocketConnected(ws);
      }
    }
  });

  return server;
}

// inject a hot reload script into the body iof an html string
const injectHotReload = (html) => {
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

  return html.replace('</body>', `${script}</body>`);
}

// a hot-reloading server for a single file
const singleFileServer = (absolutePathToFile) => {
  const file = readFile(absolutePathToFile);

  let wsClientConnection = null;

  createServer({
    onRequest: (request) => {
      return new Response(
        injectHotReload(file.text),
        {
          headers: {
            'content-type': 'text/html'
          }
        });
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

export {
  singleFileServer,
};
