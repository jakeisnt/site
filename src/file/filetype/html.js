import { SourceFile } from "file/classes";
import { readFile } from "file";

class HTMLFile extends SourceFile {
  static filetypes = ["html", "htm", "svg"];

  static create(filePath) {
    // if we have the html, just return it
    if (filePath.exists()) {
      return new HTMLFile(filePath);
    }

    // otherwise, try to get the non-html version of the file
    const path = filePath.toString().replace(".html", "");

    const prevFile = readFile(path);
    const sourceFile = prevFile.clone(filePath);

    sourceFile.asHtml = (args) => {
      return prevFile.asHtml(args);
    };

    Object.defineProperty(sourceFile, "mimeType", {
      get() {
        return "text/html";
      },
    });

    return sourceFile;
  }

  // the dependencies of an html file are the internal links
  // in the file's html.
  dependencies(settings) {
    return this.asHtml(settings).dependencies();
  }
}

export default HTMLFile;
