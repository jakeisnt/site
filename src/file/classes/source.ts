import TextFile from "./text";
import { renderArticle } from "./utils";
import { HtmlPage } from "../../html";
import { PageSettings } from "../../types/site";

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'
// same args as above fn, but without the articleHtml.

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
  protected fakeFileOf: SourceFile;

  asHtml(settings: PageSettings) {
    const page = renderSourceFile({
      file: this,
      ...settings,
    });

    return HtmlPage.create(page, settings);
  }
}

export default SourceFile;
