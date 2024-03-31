import TextFile from "./text";
import { renderArticle } from "./utils";
import { HtmlPage } from "../../html";
import { PageSettings } from "../../types/site";

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'

// same args as above fn, but without the articleHtml
const renderSourceFile = ({
  file,
  ...settings
}: PageSettings & { file: any }) => {
  const articleHtml = [
    "pre",
    ["code", { class: `language-${file.extension} has-raw-code` }, file.text],
  ];

  return renderArticle({
    file,
    articleHtml,
    ...settings,
  });
};

class SourceFile extends TextFile {
  protected fakeFileOf;

  asHtml(settings: PageSettings) {
    const page = renderSourceFile({
      file: this,
      ...settings,
    });

    return HtmlPage.create(page, settings);
  }
}

export default SourceFile;
