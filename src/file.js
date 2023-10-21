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
    // if the file doesn't exist, throw an error

    if (!fs.existsSync) {
      throw new Error(`from File constructor: File at path '${path}' does not exist`);
    }

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

  // get the string of the folder the path is contained in
  get directory() {
    const parts = filePath.split('/');
    return parts.slice(0, -1).join('/');
  }

  // I hope the file is not a directory
  get isDirectory() {
    return false;
  }

  // write this file to disk at the path
  write() {
    fs.writeFileSync(this.path, this.asString);
    return this;
  }

  // try to move this file to a new location, err otherwise
  // update this File's path to the new location
  move(toPath) {
    const fromPath = this.path;
    const cwd = this.directory:

    const { stdout, stderr } = await exec(`mv "${fromPath}" "${toPath}"`, { cwd });

    console.log(stdout);
    console.error(stderr);

    return this;
  }

  // copy this file to a new path, returning the new File
  // if the file already exists at the new path, throw an error
  // if the path doesn't exist, throw an error
  copy() {
    const fromPath = this.path;
    const toPath = path.join(this.directory, toPath);

    const cwd = this.directory:

    const { stdout, stderr } = await exec(`cp "${fromPath}" "${toPath}"`, { cwd });

    console.log(stdout);
    console.error(stderr);

    return new File(toPath);
  }
}

// a directory is a file that contains other filesq
class Directory extends File {
  // get all the files in this dir, and those dirs, and those as a flat array
  tree() {
    const dir = this.path;
    const filePaths = fs.readdirSync(dir, { withFileTypes: true });
    const childFiles = [];

    filePaths.forEach((filePath) => {
      // TODO: how we get the path to the file depends on the name
      // okay, a file is a Dirent. we can find docs for this.
      const file = new File(path.join(dir, filePath));
      if (file.isDirectory) {
        result.push(...tree(filePath));
      } else {
        result.push(filePath);
      }
    });

    return childFiles;
  }

  // get all the files in the immediate dir
  contents() {
    const filesInDir = fs.readdirSync(this.path, { withFileTypes: true });
    return filesInDir.map((file) => {
      return new File(path.join(this.path, file));
    });
  }

  get isDirectory() {
    return true;
  }
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

async function removeDir(path, inDir) {
  const { stdout, stderr } = await exec(`rm -r "${path}"`, { cwd: inDir });
  console.log(stdout);
  console.error(stderr);
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
