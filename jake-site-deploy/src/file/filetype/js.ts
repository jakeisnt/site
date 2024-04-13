import { SourceFile } from "file/classes";
import ts from "typescript";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";

const tsToJs = (tsText: string) => {
  const options = { compilerOptions: { module: ts.ModuleKind.ESNext } };
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

    sourceFile.write = (config) => {
      const { sourceDir, targetDir } = config;

      const targetPath = sourceFile.path.relativeTo(sourceDir, targetDir);

      targetPath.writeString(sourceFile.text);

      // also write the previous file
      newFile.write(config);

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
        return tsToJs(newFile.text);
      },
    });

    Object.defineProperty(sourceFile, "mimeType", {
      get() {
        return "text/javascript";
      },
    });

    Object.defineProperty(sourceFile, "require", {
      get() {
        return require(tsPath.toString());
      },
    });

    return sourceFile as JavascriptFile;
  }
}

export default JavascriptFile;