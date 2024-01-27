import { SourceFile } from "file/classes";
import { readFile } from "file";

class HTMLFile extends SourceFile {
  static filetypes = ["html", "htm", "svg"];

  // write a file to a path at the provided config location
  // when writing the html file,
  // we also write the non-html file if the file was faked.
  write(config) {
    const { sourceDir, targetDir } = config;
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.serve(config).contents);

    // write the fake file if it exists also
    this.fakeFileOf?.write(config);

    return this;
  }

  static create(filePath) {
    // if we have the html, just return it
    if (filePath.exists()) {
      return new HTMLFile(filePath);
    }

    // otherwise, try to get the non-html version of the file.
    // if the path is a directory, this won't work;
    // we then get the containing directory if this fails.
    const path = filePath.toString().replace(".html", "");
    const directoryPath = filePath.parent;

    let prevFile;
    try { 
      prevFile = readFile(path);
     } catch (e) {
      prevFile = readFile(directoryPath);
    };

    const sourceFile = prevFile.clone(filePath);

    // now, we override the new file to act like an html file.
    sourceFile.fakeFileOf = prevFile;

    sourceFile.asHtml = prevFile.asHtml;
    sourceFile.read = prevFile.asHtml;

    sourceFile.serve = (args) => {
      const contents = prevFile.asHtml(args).toString();
      return { contents, mimeType: "text/html" };
    };

    sourceFile.dependencies = (settings) => {
      return prevFile.asHtml(settings).dependencies();
    }

    // the path of this new source file needs to resolve to the html path
    Object.defineProperty(sourceFile, "path", {
      get() {
        return filePath;
      },
    });

    // the mime type of this new source file needs to be html
    Object.defineProperty(sourceFile, "mimeType", {
      get() {
        return "text/html";
      },
    });

    // produce this new source file.
    return sourceFile;
  }

  // the dependencies of an html file are the internal links
  // in the file's html.
  dependencies(settings, filesSeenSoFar) {
    return this.asHtml(settings).dependencies();
  }
}

export default HTMLFile;
