import { SourceFile } from "file/classes";
import { readFile } from "file";
import { Path } from "utils/path";
import type { PageSettings } from "../../types/site";
import { File } from "file/classes";
import { wrapFile } from "../classes/utils";

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
    targetPath.writeString(this.serve(config).contents);

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

    // Otherwise, try to get the non-html version of the file.
    // if the path is a directory, this won't work;
    // we then get the containing directory if this fails.
    const path = Path.create(filePath.toString().replace(".html", ""));

    let prevFile: File;
    try {
      prevFile = readFile(path);
    } catch (e) {
      const directoryPath = filePath.parent;
      prevFile = readFile(directoryPath);
    }

    // may have to overwrite '.read' instead of '.text'
    // so we have access to the run configuration.
    // alternatively we need `.text` to always accept the run configuration.
    // let's go with the latter.

    // we also need `wrapFile` to retrieve dependencies! that should be an argument.

    // sourceFile.read = (...args) => prevFile?.asHtml?.(...args).toString() ?? "";

    return wrapFile(prevFile, (f) => f.asHtml().toString(), path, {
      extension: "html",
      mimeType: "text/html",
    });

    sourceFile.dependencies = (settings: PageSettings) => {
      // @ts-ignore
      return prevFile?.asHtml?.(settings)?.dependencies() ?? [];
    };
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
