// Utility functions for generating HTML tags.

import { PageSyntax } from "../types/html";

const meta = (key, value): PageSyntax => {
  return ["meta", { name: key, content: value }];
};

const prop = (key, value): PageSyntax => {
  return ["meta", { property: key, content: value }];
};

type HtmlOptions = {};

const script = (src: string, opts?: HtmlOptions): PageSyntax => {
  return ["script", { src, id: src, ...opts }];
};

const css = (src: string, opts?: HtmlOptions, body?: string) => {
  return [
    "link",
    { rel: "stylesheet", type: "text/css", href: src, id: src, ...opts },
    body,
  ];
};

const favicons = (iconDir: string) => {
  return [
    [
      "link",
      { rel: "icon", type: "image/x-icon", href: iconDir + "/favicon.ico" },
    ],
    [
      "link",
      { rel: "apple-touch-icon", href: iconDir + "/apple-touch-icon.png" },
    ],
  ];
};

const colorScheme = (
  schemeName: string,
  backgroundColor: string
): PageSyntax => {
  return [
    "meta",
    {
      name: "theme-color",
      media: `(prefers-color-scheme: ${schemeName})`,
      content: backgroundColor,
    },
  ];
};

// theme headers
const theme = (): PageSyntax[] => {
  return [colorScheme("light", "white"), colorScheme("dark", "#111")];
};

const openGraphHeaders = ({ title, rootUrl, siteName }): PageSyntax[] => {
  return [
    prop("og:title", title),
    prop("og:type", "website"),
    prop("og:url", rootUrl),
    prop("og:site_name", siteName),
  ];
};

// header we can use for every page
const header = ({
  title,
  rootUrl,
  siteName,
  resourcesDir: maybeResource,
  faviconsDir: maybeFaviconsDir,
}): PageSyntax => {
  const resourcesDir = maybeResource ?? "/resources";
  const faviconsDir = maybeFaviconsDir ?? resourcesDir + "/favicon";

  return [
    "head",
    ["meta", { charset: "utf-8" }],
    ["title", `${title} / ${siteName}`],
    ...openGraphHeaders({ title, rootUrl, siteName }),
    meta("keywords", "jake"),
    meta("author", "Jake Chvatal"),
    meta("robots", "index,follow"),
    meta("description", "hi"),
    ...theme(),
    ...(resourcesDir ? favicons(faviconsDir) : []),
    css("/resources/style.css"),
    css("/resources/global.css"),
    script("/resources/lib.js"),
    css("/resources/elements.css"),
    script("/resources/elements.js", { defer: true }),

    // TODO: generate manifest.
    ["link", { rel: "manifest", href: "/resources/manifest.json" }],

    script(
      "https://unpkg.com/@highlightjs/cdn-assets@11.7.0/highlight.min.js",
      { defer: true }
    ),
    css(
      "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/nord.min.css",
      { id: "dark-theme-highlight" }
    ),
    css(
      "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/atom-one-light.min.css",
      { id: "light-theme-highlight" }
    ),
  ];
};

export { header };
