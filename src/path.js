import path from 'path';
import fs from 'fs';

// a Path is the path to a file or directory on a system.
class Path {
  // this params are immutable, so it's safe to store both and use directly
  pathArray = [];
  pathString = '';

  // this should always be an absolute path
  constructor(pathString) {
    this.pathString = pathString;
    this.pathArray = pathString.split('/').slice(1);
  }

  get toString() {
    return this.pathString;
  }

  // the name of the file includes the extension
  get name() {
    return this.pathArray[this.pathArray.length - 1];
  }

  get extension() {
    return path.extname(this.pathString);
  }

  get parent() {
    return new Path(this.pathArray.slice(0, this.pathArray.length - 1).join('/'));
  }

  // does this path exist on disk?
  exists() {
    return fs.existsSync(this.path);
  }

  replaceExtension(extension) {
    const newExtension = this.pathString.replace(/\.\S+$/, `.${extension}`);
    return new Path(newExtension);
  };

  // is this path a directory?
  isDirectory() {
    return fs.lstatSync(this.pathString).isDirectory();
  }

  // read this path as a utf8 string
  readString() {
    return fs.readFileSync(this.pathString, 'utf8');
  }

  // write a string to this path, creating the file if it doesn't exist
  writeString(str) {
    this.make();
    fs.writeFileSync(this.pathString, str);
  }

  readBinary() {
    return fs.createReadStream(this.pathString);
  }

  writeBinary(fromStream) {
    const outStream = fs.createWriteStream(outPath);
    inStream.pipe(outStream);
    await new Promise((resolve) => {
      outStream.on('close', resolve);
    });
  }

  // make this path exist, creating any parent directories along the way
  make() {
    if (this.exists()) {
      return;
    }

    const parent = this.parent;
    if (!parent.exists()) {
      parent.make();
    }

    fs.mkdirSync(this.pathString);
  }

  // TODO: return a new path with the directories provided replaced with new ones
  // for example: replaceDirectories(['foo', 'bar'], ['baz', 'qux']) would replace
  replaceDirectories(oldDirectories, newDirectories) {

  }
}

export { Path };
