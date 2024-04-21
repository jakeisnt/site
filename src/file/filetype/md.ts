import { TextFile } from "file/classes";
import type { PageSettings } from "../../types/site";

const fileHasTitle = (fileString: string) => {
  return fileString.indexOf("# ") === 0;
};

/**
 * Render the markdown article as HTML.
 */
const renderArticle = (
  mdArticle: any,
  config: PageSettings & { title: string; siteName: string }
) => {
  const pageName = mdArticle.name;

  return [
    "html",
    { lang: "en" },
    ["header", config],
    [
      "body",
      ["Sidebar"],
      [
        "div",
        { class: "site-body" },
        [
          "main",
          [
            "article",
            { class: "wikipage" },
            fileHasTitle ?? ["h1", { class: "title-top", pageName }],
            mdArticle,
          ],
        ],
        [
          "div",
          { class: "article-rhs-container" },
          [
            "div",
            { class: "article-rhs" },
            ["PageMap"],
            ["GitHistoryTable"],
            ["PrevNextUpButtons"],
          ],
        ],
      ],
    ],
  ];
};

// a CSS file is a text file
class MarkdownFile extends TextFile {
  static filetypes = ["md"];
}

export default MarkdownFile;
