import fs from 'fs';
import { exec } from '../../utils/cmd';
import { Path } from '../../utils/path';

class File {
  // the full path to the file
  path = null;

  // make the path a full path if it's not
  // if the file doesn't exist, throw an error
  constructor(path) {
    const filePath = Path.create(path);
    if (!filePath.exists()) {
      throw new Error(`from File constructor: File at path '${path}' does not exist`);
    }

    this.path = filePath;
  }

  static create(path) {
    return new this(path);
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

  // the title of the file does not
  get title() {
    return this.name.split('.')[0];
  }

  // the name of a file includes the extension
  get name() {
    return this.path.name;
  }

  // the type of the file is the extension (for now?)
  get extension() {
    return this.path.extension;
  }

  // get the mime type of the file
  get mimeType() {
    return this.path.mimeType;
  }

  // get the target extension of this file when written
  // override this if your file compiles to something of a different type?
  get targetExtension() {
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
    const cwd = this.directory;

    // const { stdout, stderr } = await exec(`mv "${fromPath}" "${toPath}"`, { cwd });

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

    // const cwd = this.directory:

    // const { stdout, stderr } = await exec(`cp "${fromPath}" "${toPath}"`, { cwd });

    console.log(stdout);
    console.error(stderr);

    return new File(toPath);
  }

  watch(callback) {
    const closeWatcher = this.path.watch((eventType, filename) => {
      callback(eventType, this);
    });

    return closeWatcher;
  }
}

export default File;
