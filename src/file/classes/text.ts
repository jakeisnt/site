import { HtmlPage } from "../../html";
import File from "./file";
import type { PageSettings } from "../../types/site";

/**
 * Represents any file that can be read as a UTF-8 string.
 */
class TextFile extends File {
  // the string contents of the file
  protected asString: string | undefined;

  read() {
    this.asString = this.path.readString();
    return this;
  }

  /**
   * Write a file to a path at the provided config location.
   */
  write(config: PageSettings) {
    const { sourceDir, targetDir } = config;
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.serve(config).contents);
    return this;
  }

  get text(): string {
    if (!this.asString) {
      this.read();
    }

    return this.asString as string;
  }

  toString() {
    return this.text;
  }

  asHtml(settings: PageSettings) {
    return HtmlPage.create(
      ["Article", { file: this, ...settings }, ["SourceBlock", { file: this }]],
      settings
    );
  }

  serve(settings: PageSettings) {
    return { contents: this.text, mimeType: this.mimeType };
  }
}

export default TextFile;
