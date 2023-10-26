import pathLibrary from 'path';
import fs from 'fs';
import mime from 'mime';
import { Repo } from './git';

import { cond } from './match';

const stringToPath = (maybePath) => {
  return cond(
    [(v) => typeof v === 'string', (v) => new Path(otherPath)]
    [true, (v) => v]
  )(maybePath);
}

// a Path is the path to a file or directory on a system.
class Path {
  // this params are immutable, so it's safe to store both and use directly
  pathArray = [];
  pathString = '';
  parentPath = null;

  // this should always be an absolute path
  // this should only be called by the static methods
  constructor(pathString) {
    if (typeof pathString !== 'string') {
      throw new Error('Path must be a string');
    }

    this.pathString = pathString;
    this.pathArray = pathString.split('/').slice(1);
  }

  static create(maybePathString) {
    if (typeof maybePathString === 'string') {
      return new Path(maybePathString);
    } else if (maybePathString instanceof Path) {
      return maybePathString;
    } else {
      throw new Error(`Path provided must be a string or Path, given ${maybePathString}`);
    }
  }

  // returns a new Path with the proper full path
  static fromUrl(url, websiteName, sourcePath) {
    return new Path(url.replace(websiteName, sourcePath));
  }

  toString() {
    return this.pathString;
  }

  // the name of the file includes the extension
  get name() {
    return this.pathArray[this.pathArray.length - 1];
  }

  // get the mime type of the file
  get mimeType() {
    return mime.getType(this.pathString) || 'text/plain';
  }

  // get this file's extension
  // if we don't have an extension, we find it
  get extension() {
    // we always fetch [1], because if the file has multiple extensions
    // we ignore the second and only care about the first.
    const ext = pathLibrary.extname(this.pathString).split('.')[1];

    if (ext) {
      return ext;
    } else if (this.isDirectory()) {
      return 'directory';
    } else {
      // TODO: what should the file look like if we don't have an extension and it
      // s not a directory?
      return ext;
    }
  }

  get parent() {
    // cache the parent path because it's probably cheaper
    if (!this.parentPath) {
      this.parentPath = new Path(this.pathArray.slice(0, this.pathArray.length - 1).join('/'));
    }
    return this.parentPath;
  }

  isRootPath() {
    return this.pathArray.length === 0;
  }

  // repo helper function
  __repo() {
    if (this.isRootPath()) {
      return null;
    }

    if (!this.isDirectory()) {
      return this.parent.repo;
    }

    const gitDir = this.join('.git');
    if (gitDir.exists()) {
      return Repo.create(this);
    } else {
      return this.parent.repo;
    }
  }

  // get the git repo that this path is a member of
  get repo() {
    if (!this.exists()) {
      return null;
    }

    return this.__repo();
  }

  // get this path's position relative to another path or string
  // ASSUME that the other paths, if defined, are the prefix of this one
  relativeTo(maybeOtherPath, maybeReplaceWithPath) {
    const otherPath = stringToPath(maybeOtherPath);
    const replaceWithPath = stringToPath(maybeReplaceWithPath);

    // assuming the other path is the prefix of this one,
    // remove it from this path
    let curPathArray = [...this.pathArray];
    otherPath.pathArray.forEach((prefixFolderName) => {
      if (!(curPathArray[0] === prefixFolderName)) {
        throw new Error(`'${otherPath.pathString}' is not a prefix of this path, '${this.pathString}'`);
      }
      curPathArray.shift();
    });

    if (replaceWithPath) {
      curPathArray = [...replaceWithPath.pathArray, curPathArray];
    }

    return new Path(curPathArray.join('/'));
  }

  exists() {
    return fs.existsSync(this.pathString);
  }

  replaceExtension(extension) {
    const newPathWithExtension = this.pathString.replace(/\.\S+$/, `.${extension}`);
    return new Path(newPathWithExtension);
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
    // TODO
    // await new Promise((resolve) => {
    //   outStream.on('close', resolve);
    // });
  }

  // read a directory, returning the directory paths
  readDirectory() {
    if (!this.isDirectory()) {
      throw new Error(`Cannot read directory '${this.pathString}' because it is not a directory`);
    }

    return fs.readdirSync(this.pathString)
      .map((fileName) => new Path(`${this.pathString}${fileName}`));
  }

  join(nextPart) {
    return new Path(this.pathString + nextPart.toString());
  }

  // does this path end with the other path
  endsWith(str) {
    return this.pathString.endsWith(str);
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

    return this;
  }

  watch(callback) {
    if (!this.exists()) {
      throw new Error(`Cannot watch path '${this.pathString}' because it does not exist`);
    }

    const watcher = fs.watch(this.pathString, (eventType, filename) => {
      // TODO: filename could be different if the file moves?
      callback(eventType, this);
    });

    return () => watcher.close();
  }
}

export { Path };
