import type { PageSyntax } from "../../types/html";
import type { PageSettings } from "../../types/site";
import TextFile from "./text";

/**
 * Render an 'article' using some standard style configuration.
 * An article is usually a source code file.
 */
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
    ["Header", { title, rootUrl, siteName, resourcesDir, faviconsDir }],
    [
      "body",
      ["Sidebar", { path: file.path, title, rootUrl, sourceDir }],
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
            ["GitHistoryTable", { file }],
            ["PrevNextUpButtons", { file, rootUrl, sourceDir }],
          ],
        ],
      ],
    ],
  ];
};

export { renderArticle };
