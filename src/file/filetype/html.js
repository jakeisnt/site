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

    // otherwise, try to get the non-html version of the file
    const path = filePath.toString().replace(".html", "");

    const prevFile = readFile(path);
    const sourceFile = prevFile.clone(filePath);

    sourceFile.fakeFileOf = prevFile;

    // now, we override the new file to act like an html file.
    sourceFile.asHtml = (args) => {
      return prevFile.asHtml(args);
    };

    sourceFile.read = (args) => {
      return prevFile.asHtml(args);
    };

    sourceFile.serve = (args) => {
      const contents = prevFile.asHtml(args).toString();
      return { contents, mimeType: 'text/html' };
    };

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
  dependencies(settings) {
    return this.asHtml(settings).dependencies();
  }
}

export default HTMLFile;
