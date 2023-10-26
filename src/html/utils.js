import { html } from './dsl';

const meta = (key, value) => {
  return ['meta', { name: key, content: value }];
}

const prop = (key, value) => {
  return ['meta', { property: key, content: value }];
}

const script = (src, opts) => {
  return ["script", { src, id: src, ...opts}];
}

const css = (src, opts, body) => {
  return ["link", { rel: 'stylesheet', type: 'text/css', href: src, id: src, ...opts}, body];
}

const favicons = (iconDir) => {
  return [
    ["link", { rel: 'icon', type: 'image/x-icon', href: iconDir + 'favicon.ico' }],
    ["link", { rel: 'apple-touch-icon', href: iconDir + 'apple-touch-icon.png' }],
  ]
}

const colorScheme = (schemeName, backgroundColor) => {
  return ["meta", { name: "theme-color", media: `(prefers-color-scheme: ${schemeName})`, content: backgroundColor }];
}

// theme headers
const theme = () => {
  return [
    colorScheme("light", "white"),
    colorScheme("dark", "#111"),
  ]
}

const openGraphHeaders = ({ title, url, siteName }) => {
  return [
    prop('og:title', title),
    prop('og:type', 'website'),
    prop('og:url', url),
    prop('og:site_name', siteName),
  ];
}

// header we can use for every page
const header = ({ path, title, rootUrl: url, siteName, iconDir }) => {
  return [
    "head",
    ["meta", { charset: 'utf-8' }],
    ["title", title],
    ...openGraphHeaders({ title, url, siteName }),
    meta('keywords', 'jake'),
    meta('author', 'Jake Chvatal'),
    meta('robots', 'index,follow'),
    meta('description', 'hi'),
    ...theme(),
    ...(iconDir ? favicons(iconDir) : []),
    css("/resources/style.css"),
    css("/resources/global.css"),
    script("/resources/lib.js"),
    css("/resources/elements.css"),
    script("/resources/elements.js", { defer: true }),

    ["link", { rel: "manifest", href: "/manifest.json" }],

    script("https://unpkg.com/@highlightjs/cdn-assets@11.7.0/highlight.min.js", { defer: true }),
    css("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/nord.min.css", { id: "dark-theme-highlight" }),
    css("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/atom-one-light.min.css", { id: 'light-theme-highlight' }),
  ]
}

export { header };
