import { isObject } from "utils/object";
import { isArray } from "utils/array";
import {
  HtmlTag,
  HtmlNode,
  HtmlTagNode,
  PageSyntax,
  HtmlAttributes,
} from "../types/html";

/**
 * Is `attrs` a set of HTML attributes`
 */
const isHtmlAttributes = (attrs: any): attrs is HtmlAttributes => {
  return isObject(attrs);
};

// Utilities for parsing the HTML DSL internal to this project.

/**
 * Get the name of a tag.
 */
const tagName = ([name]: HtmlTagNode) => name;

/**
 * Get the attributes object of a tag.
 */
const tagAttributes = ([, attributes]: HtmlTagNode) => {
  return isHtmlAttributes(attributes) ? attributes : undefined;
};

/**
 * Get the sorting rank of an HTML heading.
 */
const headingRank = (headingTag: HtmlTag) => {
  switch (headingTag) {
    case "h1":
      return 1;
    case "h2":
      return 2;
    case "h3":
      return 3;
    case "h4":
      return 4;
    case "h5":
      return 5;
    case "h6":
      return 6;
    default:
      return 7;
  }
};

const collectElements = (
  htmlPage: PageSyntax,
  predicate: (node: HtmlNode) => boolean
) => {
  // assume we only look for roots, so we return directly here lol
  if (predicate(htmlPage)) {
    return [htmlPage];
  } else if (isArray(htmlPage)) {
    // if we can act on the contents:
    const results = htmlPage.flatMap((tag) => collectElements(tag, predicate));
    return results;
  }

  return [];
};

/**
 * Find html elements with the given tag names on an html page
 */
const findTags = (htmlPage: PageSyntax, tags: HtmlTag[]) => {
  return collectElements(
    htmlPage,
    (tl) => tl && tl?.[0] && tags.includes(tl?.[0])
  );
};

/**
 * Get the link(s) embedded in an HTML tag
 * for example, the 'href' field of an <a> tag,
 * the 'link' of a 'style' tag,
 * or the 'src' of a 'script' tag.
 */
const getTagLink = (tag: HtmlTagNode) => {
  const name = tagName(tag);
  const attrs = tagAttributes(tag);

  // NOTE: There is a bug here where some attrs to an a tag are null. Not sure why.
  switch (true) {
    case ["a", "href", "link"].includes(name):
      return attrs?.href;
    case ["img", "script"].includes(name):
      return attrs?.src;
    default:
      throw new Error(
        `No link found in tag, invalid input to getLinkTag: ${tag.toString()}`
      );
  }
};

export { findTags, getTagLink };
