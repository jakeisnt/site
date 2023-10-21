import { Path } from '../utils/path';
import { splitWith } from '../utils/array';
import { case } from '../utils/match';

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

// get the fancy path name string we use for the site header
const reversePathnameForSite = (path, delimiter = ' / ') => {
  return path.pathArray.toReversed().join(delimiter);
}

// given a flat tag list of headings, make a structured hierarchy of them
// probably does not work
const makeHeadingHierarchy = (tagList, currentRank) => {
  if (!currentRank) {
    return makeHeadingHierarchy(tagList, 0);
  }

  if (!tagList.length) {
    return [];
  }

  const firstHeading = tagList[0];
  const [curHeadings, nextHeadings] = splitWith(
    tagList.slice(1), (tagArray) => headingRank(tagArray[0]) === currentRank
  );

  const firstArrayPart = case(
    [heading => currentRank === headingRank(heading[0]),
     [{
       tag: heading[0],
       id: heading[1].id,
       depth: currentRank,
       children: makeHeadingHierarchy(curHeadings, rank + 1)
     }]
    ],
    [true, makeHeadingHierarchy([firstHeading, ...curHeadings], rank + 1)]
  );

  return [
    ...firstArrayPart,
    ...headingHierarchy(nextHeadings, rank)
  ];
}

const headingHierarchyToHtml = (hierarchy) => {
  return hierarchy.map({
    tag, id, depth, children
  } => {
    if (!children?.length) {
      return [
        "div",
        { class: `hierarchy-part ${depth}` },
        ["span", { class: 'hierarchy-pipe'} "├╴"],
        ["a", { id },
         ["span", { class: 'hierarchy-text' }, id]
        ]
      ]
    }
    return ["a", { id }, headingHierarchyToHtml(children) ];
  })
}
