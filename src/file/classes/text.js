import { HtmlPage } from "../../html";
import File from "./file";
import { renderArticle } from "./utils";

const renderTextFile = ({ file, rootUrl, siteName, sourceDir }) => {
  const articleHtml = [
    "pre",
    ["code", { class: `language-${file.extension} has-raw-code` }, file.text],
  ];

  return renderArticle({ file, articleHtml, rootUrl, siteName, sourceDir });
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

  asHtml({ siteName, rootUrl, sourceDir }) {
    const page = renderTextFile({ file: this, siteName, rootUrl, sourceDir });
    return HtmlPage.create(page);
  }

  serve(args) {
    let contents = this.text;
    if (this.path.extension === 'html' && this.asHtml) {
      contents = this.asHtml(args).toString();
    } 
    return { contents, mimeType: this.mimeType };
  }
}

export default TextFile;
