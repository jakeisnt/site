const headingRank = (headingTag) => {
  switch(headingTag) {
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
}

// drop the first two elems, look through the rest
const collectElements = (htmlPage, predicate) => {
  return htmlPage.slice(2).filter(predicate);
}

// find tags with the given tag names on an html page
const findTags = (htmlPage, ...tags) {
  const tagsToLookFor = [...tags];
  return collectElements(htmlPage, ([tagName]) => {
    return tagsToLookFor.includes(tagName);
  })
}
