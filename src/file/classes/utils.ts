import { header, component } from "html";
import { PageSyntax } from "../../types/html";
import { PageSettings } from "../../types/site";
import TextFile from "./text";

const renderArticle = ({
  articleHtml,
  file,
  siteName,
  rootUrl,
  sourceDir,
  resourcesDir,
  faviconsDir,
}: PageSettings & { articleHtml: PageSyntax; file: TextFile }): PageSyntax => {
  const title = file.name;

  return [
    "html",
    header({ title, rootUrl, siteName, resourcesDir, faviconsDir }),
    [
      "body",
      component("Sidebar", { path: file.path, title, rootUrl, sourceDir }),
      [
        "div",
        { class: "site-body" },
        [
          "main",
          [
            "article",
            { class: "wikipage" },
            ["h1", { class: "title-top" }, title],
            articleHtml,
          ],
        ],
        [
          "div",
          { class: "article-rhs-container" },
          [
            "div",
            { class: "article-rhs" },
            component("GitHistoryTable", { file }),
            component("PrevNextUpButtons", { file, rootUrl, sourceDir }),
          ],
        ],
      ],
    ],
  ];
};

export { renderArticle };
