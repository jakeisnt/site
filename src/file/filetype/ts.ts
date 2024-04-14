import { SourceFile } from "file/classes";
import JavascriptFile from "./js";

import { wrapFile } from "../classes/utils";

const transpiler = new Bun.Transpiler({
  loader: "ts",
});

const tsToJs = (tsFile: SourceFile) => {
  return transpiler.transformSync(tsFile.text);
};

class TypescriptFile extends SourceFile {
  public static filetypes = ["ts"];
  public static targets = ["js"];

  js() {
    return wrapFile(this, tsToJs, this.path, {
      extension: "js",
      mimeType: "text/javascript",
    }) as JavascriptFile;
  }
}

export default TypescriptFile;
