import { HtmlPage } from "../../html";
import File from "./file";
import { renderArticle } from "./utils";
import { PageSettings } from "../../types/site";

const renderTextFile = ({
  file,
  ...config
}: PageSettings & {
  file: TextFile;
}) => {
  const articleHtml = [
    "pre",
    ["code", { class: `language-${file.extension} has-raw-code` }, file.text],
  ];

  return renderArticle({
    file,
    articleHtml,
    ...config,
  });
};

/**
 * Represents any file that can be read as a UTF-8 string.
 */
class TextFile extends File {
  // the string contents of the file
  protected asString: string = null;

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
    targetPath.writeString(this.serve().contents);
    return this;
  }

  get text() {
    if (!this.asString) {
      this.read();
    }

    return this.asString;
  }

  toString() {
    return this.text;
  }

  asHtml(settings: PageSettings) {
    const page = renderTextFile({
      file: this,
      ...settings,
    });
    return HtmlPage.create(page, settings);
  }

  serve() {
    return { contents: this.text, mimeType: this.mimeType };
  }
}

export default TextFile;
