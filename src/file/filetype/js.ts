import { SourceFile } from "file/classes";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";
import type { PageSettings } from "../../types/site";

class JavascriptFile extends SourceFile {
  static filetypes = ["js"];

  /**
   * Require this file, loading in its semantic information.
   * @returns a file configuration; notably has exports like `default`.
   */
  require() {
    return require(this.path.toString());
  }

  /**
   * Write the JS file to disk.
   *
   * Also write an equivalent file without an extension
   * to support imports like `import lib from 'lib'`.
   */
  write(cfg: PageSettings) {
    const { sourceDir, targetDir } = cfg;

    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.text(cfg));

    const targetNonJSPath = targetPath.replaceExtension();
    targetNonJSPath.writeString(this.text(cfg));

    return this;
  }

  /**
   * Create this file if it exists.
   * Otherwise, dispatch to a compile source and convert it.
   */
  static create(filePath: Path, cfg: PageSettings): JavascriptFile {
    if (filePath.exists()) {
      return new JavascriptFile(filePath, cfg);
    }

    const tsPath = filePath.replaceExtension("ts");
    const typescriptFile = readFile(tsPath, cfg) as TypescriptFile;

    return typescriptFile.js(cfg);
  }
}

export default JavascriptFile;
