import { Path } from '../utils/path';
import { link } from '../utils/printstyle';

import { readFile } from '../file';

const localPort = 4242; // Your desired port
const targetDir = '/your/target/directory'; // Replace with your target directory
const localhostUrl = `http://localhost`;
const wsLocalhostUrl = `ws://localhost`;
const sourceDir = '/home/jake/site';

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

// make a response to a request for a file with the file
const fileResponse = (file, { asHtml } = { asHtml: true }) => {
  let response = isHtml(file) ? injectHotReload(file.text) : file.text;

  if (asHtml) {
    response = injectHotReload(file.asHtml());
  }

  return new Response(
    response,
    {
      headers: {
        'content-type': asHtml ? 'text/html' : file.mimeType
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

// a hot-reloading server for a single file
const singleFileServer = (absolutePathToFile) => {
  const file = readFile(absolutePathToFile);

  let wsClientConnection = null;

  createServer({
    onRequest: (request) => {
      return fileResponse(file);
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

// to make those scss files work:
// we need a general way of saying "if you request this file and it doesnt exist,
// look for a file with the same name and another extension"

// i.e.:
//   x.y.html reads file x.y
//   z.css reads (force compiles) file z.scss
//   v.js reads  (force compiles) file v.ts
//   <directory>/ is the same as directory/index.html, which reads directory/

// file extension to the superfile extension
// TODO: should be able to define this hierarchy in the file class, not here.
// I think we can use some superclass-like relationship here, right?

// If I try to create a file at a path but the path doesn't exist,
// we ask the file to create a 'parent file' of itself.
// the file will then create a parent of itself, and so on, until it reaches a file that exists.
// the parent file then knows to have to 'translate down' to the child it came from.


// 1. Ask the file to try to make itself
// 2. If the file doesn't exist, the file should
//   a. create an instance of a super file
//   b. wrap the super file in a way that allows it to be 'translated down' to the current file.
//      this implementation should live on the current file.
//   c. return the parent file
// This can continue recursively up up up until we reach a file that exists or a dead end.

// okay, but what if we're looking at a directory of source files
// and we want to compile them as html files? how do we know how to 'compile down' to what the user wants?
// this is the static site case / arguably more important than the dynamic site case.

// one approach: emulate a dynamic site's browser requesting behavior.
// start with a file that represents the root.
// compile it and determine all of its dependencies, including internal links.
// for each dependency, make a file that represents it.
// continue recursively until we have built the 'live' segment of the site;
// the part that is reachable from the root.

// support serving arbitrary files from a directory;
// this means we have to handle routing.
const directoryServer = (absolutePathToDirectory) => {
  const dir = readFile(absolutePathToDirectory);

  if (!dir.isDirectory) {
    throw new Error(`Received path '${absolutePathToDirectory}' is not a directory`);
  }

  createServer({
    onRequest: (request) => {
      // example: we receive a request for <url>/src/server/index.js
      // we want to serve the file at <absolutePathToDirectory>/src/server/index.js

      let path = withoutUrl(request.url, devUrl);
      console.log('requested path', path);

      // is this the html version of a non-html file?
      const isHtmlVersion = path.endsWith('.html') && path.extension !== 'html';
      if (isHtmlVersion) {
        // get the non-html version of the file
        path = path.toString().replace('.html', '');
      }

      // if so, we need to serve the non-html version
      let file = dir.findFile(path);

      if (!file) {
        return new Response('Not found', { status: 404 });
      }

      return fileResponse(file, { asHtml: isHtmlVersion });
    },

    onSocketConnected: (ws) => {
      console.log('socket connected');
    }
  });

}

// does this work

export { singleFileServer, directoryServer };
