import TextFile from "./text";
import { HtmlPage } from "../../html";
import type { PageSettings } from "../../types/site";

/**
 * A source code file.
 */
class SourceFile extends TextFile {
  asHtml(settings: PageSettings) {
    return HtmlPage.create(
      ["Article", { file: this, ...settings }, ["SourceBlock", { file: this }]],
      settings
    );
  }
}

export default SourceFile;
