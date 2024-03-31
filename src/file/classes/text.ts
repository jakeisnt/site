import { HtmlPage } from "../../html";
import File from "./file";
import { renderArticle } from "./utils";

const renderTextFile = ({
  file,
  rootUrl,
  siteName,
  sourceDir,
  resourcesDir,
  faviconsDir,
}) => {
  const articleHtml = [
    "pre",
    ["code", { class: `language-${file.extension} has-raw-code` }, file.text],
  ];

  return renderArticle({
    file,
    articleHtml,
    rootUrl,
    siteName,
    sourceDir,
    resourcesDir,
    faviconsDir,
  });
};

// A text file is a file that can be read as a utf-8 string
class TextFile extends File {
  // the string contents of the file
  asString = null;

  read() {
    this.asString = this.path.readString();
    return this;
  }

  // write a file to a path at the provided config location
  write(config) {
    const { sourceDir, targetDir } = config;
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(this.serve(config).contents);
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

  asHtml(settings) {
    const page = renderTextFile({
      file: this,
      ...settings,
    });
    return HtmlPage.create(page, settings);
  }

  serve(args) {
    return { contents: this.text, mimeType: this.mimeType };
  }
}

export default TextFile;
