// utils for files, mostly for rendering

import { header, component } from "html";

const renderArticle = ({
  articleHtml,
  file,
  siteName,
  rootUrl,
  sourceDir,
  resourcesDir,
  faviconsDir,
}) => {
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
