import { SourceFile } from "../classes";
import type { PageSettings } from "../../types/site";
import type JSFile from "./js";
import { wrapFile } from "../classes/utils";
import * as ts from "typescript";

const tsToJs = (tsFile: SourceFile, cfg: PageSettings) => {
  const result = ts.transpileModule(tsFile.text(cfg), {
    compilerOptions: {
      module: ts.ModuleKind.ESNext,
      target: ts.ScriptTarget.ESNext,
    },
  });

  return result.outputText;
};

class TSFile extends SourceFile {
  public static filetypes = ["ts", "tsx"];
  public static targets = ["js"];

  js(cfg: PageSettings): JSFile {
    return wrapFile(this, (f) => tsToJs(f as SourceFile, cfg), {
      extension: "js",
    }) as JSFile;
  }
}

export default TSFile;
