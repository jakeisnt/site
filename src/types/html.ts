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

type HtmlAttributes = {
  [key: string | number]: string | number | Function;
};

type HtmlNode = string | number | HtmlTagNode;
type HtmlTagNode =
  | HtmlNode[]
  | [HtmlTag]
  | [HtmlTag, ...HtmlNode[]]
  | [HtmlTag, HtmlAttributes, ...HtmlNode[]];

type PageSyntax = HtmlNode;

export type { HtmlTag, PageSyntax, HtmlAttributes };
