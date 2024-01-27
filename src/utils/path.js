import pathLibrary from "path";
import fs from "fs";
import logger from "./log";
import mime from "mime";
import { Repo } from "./git";

// a Path is the path to a file or directory on a system.
class Path {
  // this params are immutable, so it's safe to store both and use directly
  pathArray = [];
  pathString = "";
  parentPath = null;

  // works for both relative and absolute paths, fixing them into absolute paths as needed
  // this should only be called by the static methods
  constructor(pathString) {
    console.log('constructing an awesome path string that we probably mess up', pathString);
    let normalizedPath = pathLibrary.normalize(pathString);

    if (!pathLibrary.isAbsolute(normalizedPath)) {
      normalizedPath = pathLibrary.resolve(normalizedPath, process.cwd());
    }

    this.pathString = normalizedPath;
    this.pathArray = normalizedPath.split("/").slice(1);
  }

  static create(maybePathString) {
    if (typeof maybePathString === "string") {
      return new Path(maybePathString);
    } else if (maybePathString instanceof Path) {
      return maybePathString;
    } else {
      throw new Error(
        `Path provided must be a string or Path, given ${maybePathString}`
      );
    }
  }

  // returns a new Path with the proper full path
  static fromUrl(url, websiteName, sourcePath) {
    return new Path(url.replace(websiteName, sourcePath));
  }

  toString() {
    return this.pathString;
  }

  // this path is equal to another path if they have the same pathString
  // if the other path is a string, this is still true
  equals(otherPath) {
    return this.pathString === (otherPath?.pathString ?? otherPath);
  }

  // the name of the file includes the extension
  get name() {
    return this.pathArray[this.pathArray.length - 1];
  }

  // get the mime type of the file
  get mimeType() {
    return mime.getType(this.pathString) || "text/plain";
  }

  // get this file's extension
  // if we don't have an extension, we find it
  get extension() {
    // we always fetch [1], because if the file has multiple extensions
    // we ignore the second and only care about the first.
    const ext = pathLibrary.extname(this.pathString).split(".")[1];

    if (ext) {
      return ext;
    } else if (this.exists() && this.isDirectory()) {
      return "dir";
    } else {
      // TODO: what should the file look like if we don't have an extension and it
      // s not a directory?
      return ext;
    }
  }

  get parent() {
    return Path.create(
      "/" + this.pathArray.slice(0, this.pathArray.length - 1).join("/")
    );
  }

  isRootPath() {
    // console.log('isRootPath', this.pathString, this.pathArray, this.pathString === '/');
    // NOTE: here, this.pathArray is [""] for some reason?
    return this.pathString === "/";
  }

  // repo helper function
  __repo() {
    if (this.isRootPath()) {
      return null;
    }

    if (!this.isDirectory()) {
      return this.parent.__repo();
    }

    const gitDir = this.join("/.git");

    if (gitDir.exists()) {
      return Repo.create(this);
    } else {
      return this.parent.__repo();
    }
  }

  // get the git repo that this path is a member of
  get repo() {
    if (!this.exists()) {
      return null;
    }

    const rootRepo = this.__repo();
    if (rootRepo) {
      return rootRepo;
    }
  }

  // get this path's position relative to another path or string
  // ASSUME that the other paths, if defined, are the prefix of this one
  relativeTo(maybeOtherPath, maybeReplaceWithPath) {
    const otherPath = Path.create(maybeOtherPath);
    const replaceWithPath = maybeReplaceWithPath ? Path.create(maybeReplaceWithPath) : '';

    // assuming the other path is the prefix of this one,
    // remove it from this path
    let curPathArray = [...this.pathArray];
    otherPath.pathArray.forEach((prefixFolderName) => {
      if (!(curPathArray[0] === prefixFolderName)) {
        throw new Error(
          `'${otherPath.pathString}' is not a prefix of this path, '${this.pathString}'`
        );
      }
      curPathArray.shift();
    });

    if (replaceWithPath) {
      curPathArray = [...replaceWithPath.pathArray, ...curPathArray];
    }

    const pathAfterShift = Path.create("/" + curPathArray.join("/"));
    return pathAfterShift;
  }

  exists() {
    return fs.existsSync(this.pathString);
  }

  replaceExtension(extension) {
    const newPathWithExtension = this.pathString.replace(
      /\.\S+$/,
      `.${extension}`
    );
    return new Path(newPathWithExtension);
  }

  // is this path a directory?
  isDirectory() {
    if (!this.exists()) {
      throw new Error("Cannot check if a path is a directory if it doesn't exist", this.pathString);
    }

    return fs.lstatSync(this.pathString).isDirectory();
  }

  // read this path as a utf8 string
  readString() {
    return fs.readFileSync(this.pathString, "utf8");
  }

  // write a string to this path, creating the file if it doesn't exist
  writeString(str) {
    this.make();
    fs.writeFileSync(this.pathString, str);
  }

  readBinary() {
    const chunks = [];
    let buffer;

    // const readStream = fs.createReadStream(this.pathString);
    // readStream.on('data', (chunk) => {
    //   chunks.push(chunk);
    // });

    // readStream.on('end', () => {
    //   buffer = Buffer.concat(chunks);
    // });

    // while (!buffer) {
    // }
    return buffer;
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
      throw new Error(
        `Cannot read directory '${this.pathString}' because it is not a directory`
      );
    }

    let normalizedPathString = this.pathString;

    // if the path doesn't end in a slash, add one
    if (normalizedPathString[normalizedPathString.length - 1] !== "/") {
      normalizedPathString += "/";
    }

    return fs
      .readdirSync(normalizedPathString)
      .map((fileName) => new Path(`${normalizedPathString}${fileName}`));
  }

  join(nextPart) {
    logger.file("Joining path", this.pathString, "with", nextPart.toString());
    return new Path(this.pathString + nextPart.toString());
  }

  // does this path end with the other path
  endsWith(str) {
    return this.pathString.endsWith(str);
  }

  // not exactly true lol..
  contains(maybeOtherPathString) {
    const otherPath = Path.create(maybeOtherPathString);

    if (
      this.pathArray.length > otherPath.pathArray.length ||
      this.pathString === otherPath.pathString
    ) {
      return false;
    }

    for (let i = 0; i < this.pathArray.length; i++) {
      if (otherPath.pathArray[i] !== this.pathArray[i]) {
        return false;
      }
    }

    return true;
  }

  // make this path exist, creating any parent directories along the way
  // assume the path is a file unless provided that it's a directory
  make({isDirectory = false} = { isDirectory: false }) {
    if (this.exists()) {
      console.log('.make: File already exists at path ', this.pathString);
      return this;
    }

    // if this file is supposed to have a parent, then,
    // by definition, its parent must be a directory.
    // make sure the parent directory exists.

    if (!this.parent.exists()) {
      console.log('The parent of this path does not exist. Making it: ', this.parent.pathString)
      this.parent.make({ isDirectory: true });
    }

    if (isDirectory) {
      console.log("Making directory at", this.pathString);
      fs.mkdirSync(this.pathString); 
    } else {
      console.log("Making file at", this.pathString);
      fs.writeFileSync(this.pathString, "");
    }

    return this;
  }

  watch(callback) {
    if (!this.exists()) {
      throw new Error(
        `Cannot watch path '${this.pathString}' because it does not exist`
      );
    }

    const watcher = fs.watch(this.pathString, (eventType, filename) => {
      // TODO: filename could be different if the file moves?
      callback(eventType, this);
    });

    return () => watcher.close();
  }
}

export { Path };
