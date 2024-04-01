import { TextFile } from "file/classes";
import { header, component } from "html";
import type { PageSettings } from "../../types/site";

// TODO
const fileHasTitle = (fileString: string) => {
  return fileString.indexOf("# ") === 0;
};

// after the article is in some html ast form, get more info from it
const renderArticle = (mdArticle: any, config: PageSettings) => {
  const pageName = mdArticle.name;

  return [
    "html",
    header(config),
    [
      "body",
      component("Sidebar"),
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
            component("PageMap"),
            component("GitHistoryTable"),
            component("PrevNextUpButtons"),
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
