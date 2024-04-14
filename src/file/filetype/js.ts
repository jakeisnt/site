import { SourceFile } from "file/classes";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";
import type { PageSettings } from "../../types/site";

class JavascriptFile extends SourceFile {
  static filetypes = ["js"];

  require() {
    return require(this.path.toString());
  }

  // Write both the .js file and the file without an extension as JS
  write(config: PageSettings) {
    const { sourceDir, targetDir } = config;

    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.text);

    const targetNonJSPath = targetPath.replaceExtension();
    console.log(targetNonJSPath.toString());
    targetNonJSPath.writeString(this.text);

    return this;
  }

  /**
   * Create this file if it exists.
   * Otherwise, dispatch to a compile source and convert it.
   */
  static create(filePath: Path): JavascriptFile {
    if (filePath.exists()) {
      return new JavascriptFile(filePath);
    }

    // If we don't have the JS file, try grabbing the TS file.
    const tsPath = filePath.replaceExtension("ts");
    const typescriptFile = readFile(tsPath) as TypescriptFile;

    return typescriptFile.asJSFile();
  }
}

export default JavascriptFile;
