import { SourceFile, TextFile } from "file/classes";
import { readFile } from "../index";
import { Path } from "../../utils/path";
import TypescriptFile from "./ts";
import type { PageSettings } from "../../types/site";

const transpiler = new Bun.Transpiler({
  loader: "ts",
});

const tsToJs = (tsText: string) => {
  return transpiler.transformSync(tsText);
};

/**
 * Convert a file, wrapping that a file in a parent.
 * The parent file dispatches to a cloned child file for info.
 */
const wrapFile = (
  sourceFile: TextFile, // the ts file
  getText: (source: string) => string,
  path: Path,
  {
    extension,
    mimeType,
  }: {
    extension: string;
    mimeType: string;
  }
) => {
  const wrappingFile = sourceFile.clone();

  wrappingFile.write = (config: PageSettings) => {
    const { sourceDir, targetDir } = config;

    // Write the wrapping file
    const targetPath = sourceFile.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(sourceFile.text);

    // write the javascript file without an extension
    // const noExtensionPath = targetPath.replaceExtension();
    // noExtensionPath.writeString(sourceFile.text);

    // Write the source file
    sourceFile.write(config);

    return wrappingFile;
  };

  Object.defineProperty(wrappingFile, "text", {
    get() {
      // There's an inneficiency. We compile on the fly every time.
      // This can be cached!
      return getText(sourceFile.text);
    },
  });

  Object.defineProperty(wrappingFile, "mimeType", {
    get() {
      return mimeType;
    },
  });

  // the path of this new source file needs to resolve to the old path
  Object.defineProperty(wrappingFile, "path", {
    get() {
      return path.replaceExtension(extension);
    },
  });

  Object.defineProperty(wrappingFile, "require", {
    get() {
      return require(path.toString());
    },
  });

  return wrappingFile; // the js file
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
