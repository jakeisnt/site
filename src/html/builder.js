import { readFile } from "../file";
import { htmlPage } from "./dsl";
import { getTagLink, findTags } from "./parseDSL";

// is the link string that we are provided internal?
const isInternalLink = (l, settings) => {
  const { rootUrl } = settings;

  if (l.includes(rootUrl)) {
    return true;
  }

  const isExternal =
    l.includes("data:image") ||
    l.includes("http://") ||
    l.includes("https://") ||
    l.startsWith("#");

  return !isExternal;
};

// Convert a link string to a legitimate file path on disk.
// probably requires more arguments
const linkStringToFile = (l, settings) => {
  const { sourceDir, rootUrl } = settings;

  // remove the leading rootUrl from the link if it exists
  const linkWithoutRoot = l
    .replace(rootUrl, "")
    .replace("http://", "")
    .replace("https://", "");

  // path the now-local url to the source dir
  return sourceDir.toString().concat(linkWithoutRoot);
};

// Represents an HTML AST that may not be associated with a file
class HtmlPage {
  pageStructure = null;
  currentBuildSettings = null;
  cachedDependencies = null;

  constructor(pageSyntax, settings) {
    this.pageStructure = pageSyntax;
    this.currentBuildSettings = settings;
  }

  static create(pageSyntax, settings) {
    return new this(pageSyntax, settings);
  }

  // Find all of the dependencies in the page.
  // We define a 'dependency' as any internal link.
  // For example -- src/index.css, src/file.html, etc.
  // Produces these dependencies as Files.
  dependencies(settings = this.currentBuildSettings) {
    if (!this.cachedDependencies) {
      this.cachedDependencies = findTags(this.pageStructure, [
        "a",
        "href",
        "img",
        "script",
        "link",
      ])
        .map(getTagLink)
        .filter((v) => v)
        .filter((f) => isInternalLink(f, settings))
        .map((f) => linkStringToFile(f, settings))
        .map(readFile);
    }

    return this.cachedDependencies;
  }

  toString() {
    return htmlPage(this.pageStructure);
  }
}

export default HtmlPage;
