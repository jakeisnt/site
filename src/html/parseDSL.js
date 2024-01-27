import { splitWith } from "utils/array";
import { isObject, isArray } from "./dsl";

// get the name of a tag.
const tagName = ([name]) => name;
// get the attributes object of a tag.
const tagAttributes = ([, attributes]) => {
  return isObject(attributes) ? attributes : null;
};

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

  const elements = [];

  if (predicate(htmlPage)) {
    elements.push(htmlPage);
  }

  // if we can act on the contents:
  if (isArray(htmlPage)) {
    return [
      ...(elements.length ? elements : []),
      ...htmlPage.map((tag) => collectElements(tag, predicate)),
    ];
  }

  // if we're at a leaf, we terminate, returning just the current element.
  return elements;
};

// find html elements with the given tag names on an html page
const findTags = (htmlPage, tags) => {
  return collectElements(htmlPage, (tl) => tl && tl?.[0] && tags.includes(tl?.[0]));
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
      throw new Error("No link found in tag, invalid input to getLinkTag", tag);
  }
};

export { findTags, getTagLink };
