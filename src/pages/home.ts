import { header, component, HtmlPage } from "html";
import { Path } from "utils/path";

const makeHomePage = (settings) => {
  const { rootUrl, sourceDir, siteName, resourcesDir, faviconsDir } = settings;
  const title = "index";
  const htmlText = [
    "html",
    header({ title, rootUrl, siteName, resourcesDir, faviconsDir }),
    [
      "body",
      // (components/component "sidebar" {:target-path "/index.html"} nil nil [[:h1 "~"]])
      component("Sidebar", {
        path: Path.create(sourceDir),
        title,
        rootUrl,
        sourceDir,
      }),
      [
        "div",
        { class: "site-body" },
        [
          "main",
          [
            "article",
            { class: "wikipage aboutMe" },
            ["p", "Hey, I'm Jake Chvatal."],
            ["p", "I'm a software engineer based in Stockholm, Sweden."],
            [
              "p",
              "During the day, I work at ",
              [
                "a",
                { class: "external", href: "https://improvin.com" },
                "Improvin'",
              ],

              ", building tools to help food companies reduce their environmental impact.",
            ],
            [
              "p",
              "On nights and weekends, I ",
              ["a", { class: "internal", href: "/pages/index.html" }, "write"],
              ", take ",
              [
                "a",
                { class: "external", href: "https://instagram.com/jakeisnt" },
                "photos",
              ],
              ", and design simple hardware and software tools.",
            ],
            component("LastFM"),
          ],
        ],
      ],
    ],
  ];

  return HtmlPage.create(htmlText, settings);
};

export { makeHomePage };
