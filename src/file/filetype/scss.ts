import { SourceFile } from "file/classes";

import { sourceDir } from "../../constants";
import * as sass from "sass";
const { pathToFileURL } = require("url");
import { wrapFile } from "../classes/utils";
import type CSSFile from "./css";

/**
 * Convert provided SCSS text to a CSS string.
 */
const scssToCss = (scssFile: SourceFile) => {
  const result = sass.compileString(scssFile.text, {
    sourceMap: false,
    importers: [
      {
        findFileUrl(url) {
          const nextUrl = new URL(url, pathToFileURL(`${sourceDir}/`));
          return nextUrl;
        },
      },
    ],
  });

  return result.css.toString();
};

class SCSSFile extends SourceFile {
  public static filetypes = ["scss", "sass"];
  public static targets = ["css"];

  css() {
    return wrapFile(this, scssToCss, {
      extension: "css",
      mimeType: "text/css",
    }) as CSSFile;
  }
}

export default SCSSFile;
