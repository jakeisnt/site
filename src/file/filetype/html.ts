import { SourceFile } from "file/classes";
import { readFile } from "file";
import { Path } from "utils/path";
import { PageSettings } from "../../types/site";

class HTMLFile extends SourceFile {
  static filetypes = ["html", "htm", "svg"];

  /**
   * Write a file to a path at the provided config location.
   * When writing the html file,
   * we also write the non-html file if the file was faked.
   */
  write(config: PageSettings) {
    const { sourceDir, targetDir } = config;
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.serve().contents);

    // write the fake file if it exists also
    this.fakeFileOf?.write(config);

    return this;
  }

  /**
   * Create an HTML file from path.
   */
  static create(filePath: Path) {
    // if we have the html, just return it
    if (filePath.exists()) {
      return new HTMLFile(filePath);
    }

    // otherwise, try to get the non-html version of the file.
    // if the path is a directory, this won't work;
    // we then get the containing directory if this fails.
    const path = Path.create(filePath.toString().replace(".html", ""));
    const directoryPath = filePath.parent;

    let prevFile;
    try {
      prevFile = readFile(path);
    } catch (e) {
      prevFile = readFile(directoryPath);
    }

    const sourceFile = prevFile.clone();

    // now, we override the new file to act like an html file.
    // NOTE: all of the question marks are hacks.
    // some files are too eagerly linked as html,
    // so we ignore them and create spurious html here with no contents.
    // like image files.
    // really, those files should not be linked to as html at all.
    sourceFile.fakeFileOf = prevFile;
    sourceFile.asHtml = prevFile.asHtml;
    sourceFile.read = (...args) => prevFile?.asHtml?.(...args).toString() ?? "";

    sourceFile.write = (config) => {
      const { sourceDir, targetDir } = config;

      const targetPath = sourceFile.path.relativeTo(sourceDir, targetDir);
      targetPath.writeString(sourceFile.serve(config)?.contents ?? "");

      // also write the previous file
      prevFile.write(config);

      return sourceFile;
    };

    sourceFile.serve = (...args) => {
      const contents = prevFile?.asHtml?.(...args).toString() ?? "";
      return { contents, mimeType: "text/html" };
    };

    sourceFile.dependencies = (settings) => {
      return prevFile?.asHtml?.(settings)?.dependencies() ?? [];
    };

    // the path of this new source file needs to resolve to the html path
    Object.defineProperty(sourceFile, "path", {
      get() {
        return filePath;
      },
    });

    // the path of this new source file needs to resolve to the html path
    Object.defineProperty(sourceFile, "isDirectory", {
      get() {
        return false;
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

  /**
   * Find the dependencies of the HTML file.
   *
   * The dependencies of an html file are the internal links
   * in the file's html.
   */
  dependencies(settings: PageSettings) {
    return this.asHtml(settings).dependencies();
  }
}

export default HTMLFile;
