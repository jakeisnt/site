import http from 'http';
import fs from 'fs';
import path from 'path';
import mime from 'mime';
import { Path } from '../utils/path';

import { readFile } from '../file';

const constPort = 4242; // Your desired port
const targetDir = '/your/target/directory'; // Replace with your target directory
const localhostUrl = `http://localhost:${constPort}`;
const sourceDir = '/home/jake/site';


// function getFilePath(uri) {
//   let filePath = path.join(targetDir, uri);
//   if (fs.existsSync(filePath) && fs.lstatSync(filePath).isDirectory()) {
//     filePath = path.join(filePath, 'index.html');
//   }
//   return filePath;
// }

function serveFile(response, file) {
  file.onRequest((err, data) => {
    if (err) {
      response.writeHead(404, { 'Content-Type': 'text/plain' });
      response.end('Not Found');
    } else {
      const contentType = file.type;
      response.writeHead(200, { 'Content-Type': contentType });
      response.end(data);
    }
  });
}

const server = http.createServer((request, response) => {
  const path = Path.fromUrl(url, localhostUrl, sourceDir);
  const file = readFile(request.url);
  serveFile(response, file);
});

server.listen(constPort, () => {
  console.log(`Server is running at http://localhost:${constPort}/`);
});
