const path = require('path');

// a Path is the path to a file or directory on a system.
// A Path can be changed to
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
    const newExtension = this.path.replace(/\.\S+$/, `.${extension}`);
    return new Path(newExtension);
  };

  // return a new path with the directories provided replaced with new ones
  // for example: replaceDirectories(['foo', 'bar'], ['baz', 'qux']) would replace
  replaceDirectories(oldDirectories, newDirectories) {

  }
}

export { Path };
