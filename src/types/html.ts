type HtmlTag =
  | "html"
  | "body"
  | "div"
  | "img"
  | "a"
  | "h1"
  | "h2"
  | "h3"
  | "h4"
  | "h5"
  | "h6"
  | "button"
  | "script"
  | "link"
  | "meta"
  | "table"
  | "tr"
  | "td"
  | "p"
  | "span"
  | "ul"
  | "li"
  | "ol"
  | "form"
  | "input"
  | "label"
  | "select"
  | "option"
  | "textarea"
  | "header"
  | "footer"
  | "main"
  | "nav"
  | "section"
  | "article"
  | "aside"
  | "figure"
  | "figcaption"
  | "canvas"
  | "audio"
  | "video"
  | "source"
  | "track"
  | "embed"
  | "object"
  | "param"
  | "iframe"
  | "picture"
  | "svg"
  | "path"
  | "g";

// A generic HTML attribute.
// Doesn't type specific tag names.
type GenericAttribute = { [key: string | number]: string | number | Function };

type HtmlAttributes = GenericAttribute & {
  src?: string;
  href?: string;
  class?: string;
  role?: string;
  id?: string;
};

// HTML nodes that are never rendered.
// These should be discarded if found in the tree.
type FalsyHtmlNode = undefined | null;

// An HTML node available to our DSL.
type HtmlNode = string | HtmlTagNode | HtmlNode[] | FalsyHtmlNode;

// A compound HTML tag.
// Optionally allows the user to configure the HTML with attributes.
type HtmlTagNode =
  | [HtmlTag]
  | [HtmlTag, ...HtmlNode[]]
  | [HtmlTag, HtmlAttributes, ...HtmlNode[]];

type PageSyntax = HtmlNode;

export type { HtmlTag, PageSyntax, HtmlNode, HtmlTagNode, HtmlAttributes };
