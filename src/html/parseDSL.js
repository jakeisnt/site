import { splitWith } from "utils/array";
import { isObject } from "./dsl";

const headingRank = (headingTag) => {
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

// drop the first two elems, look through the rest
const collectElements = (htmlPage, predicate) => {
  return htmlPage.slice(2).filter(predicate);
};

// find html elements with the given tag names on an html page
const findTags = (htmlPage, tags) => {
  return collectElements(htmlPage, ([tagName]) => {
    return tags.includes(tagName);
  });
};

// get the name of a tag.
const tagName = ([name]) => name;
// get the attributes object of a tag.
const tagAttributes = ([, attributes]) => {
  return isObject(attributes) ? attributes : null;
};
// get the contents of a tag.
const tagContents = ([, maybeAttributes, maybeContents]) => {
  return isObject(maybeAttributes) ? maybeContents : maybeAttributes;
};

// Get the link(s) embedded in an HTML tag
// for example, the 'href' field of an <a> tag,
// the 'link' of a 'style' tag,
// or the 'src' of a 'script' tag.
const getTagLink = (tag) => {
  switch (tagName(tag)) {
    case ("a", "href"):
      return tagAttributes(tag).href;
    case ("img", "script"):
      return tagAttributes(tag).src;
    default:
      return null;
  }
};

export { findTags, getTagLink };
