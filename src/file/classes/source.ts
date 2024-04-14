import TextFile from "./text";
import { HtmlPage } from "../../html";
import type { PageSettings } from "../../types/site";

/**
 * A source code file.
 */
class SourceFile extends TextFile {
  // If this file is 'pretending' to be another file,
  // the file that this was wrapped around is accessible here.

  // This allows us to pull tricks like asking questions about a
  // javascript file when the actual file is written in typescript,
  // converting configuration files into others on the fly,
  // reading SCSS as CSS, etc.
  public fakeFileOf: SourceFile | undefined;

  asHtml(settings: PageSettings) {
    return HtmlPage.create(
      ["Article", { file: this, ...settings }, ["SourceBlock", { file: this }]],
      settings
    );
  }
}

export default SourceFile;
