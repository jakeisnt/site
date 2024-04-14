import { SourceFile } from "file/classes";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";
import type { PageSettings } from "../../types/site";
import { wrapFile } from "../classes/utils";

const transpiler = new Bun.Transpiler({
  loader: "ts",
});

const tsToJs = (tsFile: TypescriptFile) => {
  return transpiler.transformSync(tsFile.text);
};

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

  // override the create behavior to create the parent
  static create(filePath: Path): JavascriptFile {
    if (filePath.exists()) {
      return new JavascriptFile(filePath);
    }

    const tsPath = filePath.replaceExtension("ts");
    // If we don't have the JS file, try grabbing the TS file.
    const typescriptFile = readFile(tsPath) as TypescriptFile;

    return wrapFile(typescriptFile, tsToJs, tsPath, {
      extension: "js",
      mimeType: "text/javascript",
    }) as JavascriptFile;
  }
}

export default JavascriptFile;
