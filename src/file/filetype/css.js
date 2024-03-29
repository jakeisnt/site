import { SourceFile } from "file/classes";
import * as sass from "sass";
const { pathToFileURL } = require("url");
import { readFile } from "file";
import { sourceDir } from "../../constants";

const scssToCss = (scssText) => {
  const result = sass.compileString(scssText, {
    sourceMap: false,
    importers: [
      {
        findFileUrl(url) {
          const nextUrl = new URL(url, pathToFileURL(`${sourceDir}/`));
          return nextUrl;
        },
      },
    ],
  });
  return result.css.toString();
};

// a CSS file is a text file
class CSSFile extends SourceFile {
  static filetypes = ["css"];

  // override the create behavior to create the parent
  static create(filePath) {
    if (filePath.exists()) {
      return new CSSFile(filePath);
    }

    // if this file doesn't exist, try making the scss file.
    const scssPath = filePath.replaceExtension("scss");

    // make the scss file
    // and then make a wrapper css file that automatically compiles the scss file.
    // this css file can be interfaced with just like a css file,
    // but the underlying source of truth is always the scss file.

    const prevFile = readFile(scssPath);
    const sourceFile = prevFile.clone();

    sourceFile.fakeFileOf = prevFile;

    sourceFile.write = (config) => {
      const { sourceDir, targetDir } = config;

      const targetPath = sourceFile.path.relativeTo(sourceDir, targetDir);
      targetPath.writeString(sourceFile.text);

      // also write the previous file
      prevFile.write(config);

      return sourceFile;
    };

    // the path of this new source file needs to resolve to the old path
    Object.defineProperty(sourceFile, "path", {
      get() {
        return filePath;
      },
    });

    Object.defineProperty(sourceFile, "text", {
      get() {
        return scssToCss(prevFile.text);
      },
    });

    Object.defineProperty(sourceFile, "mimeType", {
      get() {
        return "text/css";
      },
    });

    return sourceFile;
  }
}

export default CSSFile;
