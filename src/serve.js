const http = require('http');
const fs = require('fs');
const path = require('path');
const mime = require('mime');

const constPort = 4242; // Your desired port
const targetDir = '/your/target/directory'; // Replace with your target directory

function getFilePath(uri) {
  let filePath = path.join(targetDir, uri);
  if (fs.existsSync(filePath) && fs.lstatSync(filePath).isDirectory()) {
    filePath = path.join(filePath, 'index.html');
  }
  return filePath;
}

function serveFile(response, filePath) {
  fs.readFile(filePath, (err, data) => {
    if (err) {
      response.writeHead(404, { 'Content-Type': 'text/plain' });
      response.end('Not Found');
    } else {
      const contentType = mime.getType(filePath) || 'text/plain';
      response.writeHead(200, { 'Content-Type': contentType });
      response.end(data);
    }
  });
}

const server = http.createServer((request, response) => {
  const filePath = getFilePath(request.url);
  serveFile(response, filePath);
});

server.listen(constPort, () => {
  console.log(`Server is running at http://localhost:${constPort}/`);
});
