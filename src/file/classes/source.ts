import TextFile from "./text";
import { renderArticle } from "./utils";
import { HtmlPage } from "../../html";

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'

// same args as above fn, but without the articleHtml
const renderSourceFile = ({ file, rootUrl, siteName, sourceDir }) => {
  const articleHtml = [
    "pre",
    ["code", { class: `language-${file.extension} has-raw-code` }, file.text],
  ];

  return renderArticle({ file, articleHtml, rootUrl, siteName, sourceDir });
};

class SourceFile extends TextFile {
  private fakeFileOf;

  asHtml(settings) {
    const { siteName, rootUrl, sourceDir } = settings;
    const page = renderSourceFile({ file: this, siteName, rootUrl, sourceDir });

    return HtmlPage.create(page, settings);
  }
}

export default SourceFile;
