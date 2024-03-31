import { SourceFile } from "file/classes";
import * as ts from "typescript";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";

const tsToJs = (tsText: string) => {
  const options = { compilerOptions: { module: ts.ModuleKind.CommonJS } };
  return ts.transpileModule(tsText, options).outputText;
};

class JavascriptFile extends SourceFile {
  static filetypes = ["js"];

  require() {
    return require(this.path.toString());
  }

  // override the create behavior to create the parent
  static create(filePath: Path): JavascriptFile {
    if (filePath.exists()) {
      return new JavascriptFile(filePath);
    }

    // if this file doesn't exist, try making the typescript file.
    const tsPath = filePath.replaceExtension("ts");
    const newFile = readFile(tsPath) as TypescriptFile;
    const sourceFile = newFile.clone();

    Object.defineProperty(sourceFile, "text", {
      get() {
        return tsToJs(newFile.text);
      },
    });

    Object.defineProperty(sourceFile, "mimeType", {
      get() {
        return "text/javascript";
      },
    });

    return sourceFile;
  }
}

export default JavascriptFile;
