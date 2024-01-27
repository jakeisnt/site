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

const collectElements = (htmlPage, predicate) => {
  // assume we only look for roots, so we return directly here lol
  if (predicate(htmlPage)) { 
    return [htmlPage];
  } else if (isArray(htmlPage)) {
      // if we can act on the contents: 

      const results = htmlPage.flatMap((tag) => collectElements(tag, predicate));
      console.log('before', htmlPage, 'after acting on contents', results);
      return results;
  }

  return [];
};

// find html elements with the given tag names on an html page
const findTags = (htmlPage, tags) => {
  const elems = collectElements(htmlPage, (tl) => tl && tl?.[0] && tags.includes(tl?.[0]));
  return elems;
};

// Get the link(s) embedded in an HTML tag
// for example, the 'href' field of an <a> tag,
// the 'link' of a 'style' tag,
// or the 'src' of a 'script' tag.
const getTagLink = (tag) => {
  const name = tagName(tag);
  const attrs = tagAttributes(tag);

  // NOTE: there is a bug here where some attrs to an a tag are null. not sure why.

  switch (true) {
    case ["a", "href"].includes(name):
      return tagAttributes(tag)?.href;
    case ["img", "script"].includes(name):
      return tagAttributes(tag)?.src;
    default:
      throw new Error("No link found in tag, invalid input to getLinkTag", tag);
  }
};

export { findTags, getTagLink };
