const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);

// a file has three stages:
// - read
// - parsed (JSON)
// - compiled (JS)

class File {
  // the string contents of the file
  asString = null;
  // the parsed JSON contents of the file
  asAST = null;
  // the compiled HTML file
  asHtml = null;

  // the full path to the file
  path = null;

  constructor(path) {
    // make the path a full path if it's not
    this.path = path;
  }

  // read the file at the path
  read() {
    this.asString = fs.readFileSync(this.path, 'utf8');
    return this;
  }

  get text() {
    return this.read().asString;
  }

  get path() {
    return this.path;
  }

  // the title of a file is the file name with the extension
  get title() {
    const parts = this.path.split('/');
    return parts[parts.length - 1];
  }

  // the name of a file is the file name without the extension
  get name() {
    return this.title.split('.')[0];
  }

  // the extension of a file is the file name without the name
  get extension() {
    return this.title.split('.')[1];
  }
}

// TODO: image files are like files, but have to be read differently
class ImageFile extends File {}


function tree(dir) {
  const files = fs.readdirSync(dir, { withFileTypes: true });
  const result = [];
  files.forEach((file) => {
    const filePath = path.join(dir, file.name);
    if (file.isDirectory()) {
      result.push(...tree(filePath));
    } else {
      result.push(filePath);
    }
  });
  return result;
}

function listDir(dir) {
  return fs.readdirSync(dir);
}

function readImage(filePath) {
  try {
    return fs.createReadStream(filePath);
  } catch (e) {
    console.error('File read from path not found:', filePath);
    console.error(e);
    return null;
  }
}

async function writeImage(inStream, outPath) {
  const outStream = fs.createWriteStream(outPath);
  inStream.pipe(outStream);
  await new Promise((resolve) => {
    outStream.on('close', resolve);
  });
}

function makeDirectory(dir) {
  fs.mkdirSync(dir);
}

function write(content, filePath) {
  fs.writeFileSync(filePath, content, 'utf8');
}

function isDir(path) {
  return fs.statSync(path).isDirectory();
}

async function move(from, to, dir) {
  const { stdout, stderr } = await exec(`mv "${from}" "${to}"`, { cwd: dir });
  console.log(stdout);
  console.error(stderr);
}

async function copyDir(from, to, dir) {
  const { stdout, stderr } = await exec(`cp -r "${from}" "${to}"`, { cwd: dir });
  console.log(stdout);
  console.error(stderr);
}

async function copy(from, to, fromDir) {
  await copyDir(from, to, fromDir);
}

async function removeDir(path, inDir) {
  const { stdout, stderr } = await exec(`rm -r "${path}"`, { cwd: inDir });
  console.log(stdout);
  console.error(stderr);
}

function exists(filePath) {
  return fs.existsSync(filePath);
}

module.exports = {
  getPath,
  tree,
  listDir,
  read,
  readImage,
  writeImage,
  makeDirectory,
  write,
  title,
  name,
  extension,
  isDir,
  move,
  copyDir,
  copy,
  removeDir,
  exists,
};
