import fs from 'fs';
import { exec } from '../utils/cmd';
import { Path } from '../utils/path';

class File {
  // the full path to the file
  path = null;

  // make the path a full path if it's not
  // if the file doesn't exist, throw an error
  constructor(path) {
    const filePath = new Path(path);
    if (!filePath.exists()) {
      throw new Error(`from File constructor: File at path '${path}' does not exist`);
    }

    this.path = filePath;
  }

  // read the file at the path
  read() {
    throw new Error('File.read() is not implemented');
  }

  // write this file to disk at the path
  write() {
    throw new Error('File.write() is not implemented');
  }

  // what to return when this file is requested by a server or something
  onRequest() {
    throw new Error('File.onRequest() is not implemented');
  }

  get path() {
    return this.path;
  }

  get title() {
    const parts = this.path.split('/');
    return parts[parts.length - 1];
  }

  // the name of a file is the file name without the extension
  get name() {
    return this.title.split('.')[0];
  }

  // the type of the file is the extension (for now?)
  get type() {
    return this.path.extension;
  }

  // get the string of the folder the path is contained in
  get directory() {
    return this.path.parent;
  }

  // I hope the file is not a directory
  get isDirectory() {
    return false;
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
class Directory extends File {// get all the files in this dir, and those dirs, and those as a flat array
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

// A text file is a file that can be read as a utf-8 string
class TextFile extends File {
  // the string contents of the file
  asString = null;

  read() {
    this.asString = fs.readFileSync(this.path, 'utf8');
    return this;
  }

  write() {
    this.path.writeString(this.asString);
    return this;
  }

  get text() {
    if (!this.asString) {
      this.read();
    }

    return this.asString;
  }

  onRequest() {
    return this.text;
  }
}

// A BinaryFile is a file that we can't read or write as a string (like an image)
class BinaryFile extends File {
  // the binary contents of the file
  asBinary = null;

  read() {
    this.asBinary = this.path.readBinary();
    return this;
  }

  write() {
    this.path.writeBinary(this.asBinary);
    return this;
  }
}

export {
  Directory,
  TextFile,
  BinaryFile
};
