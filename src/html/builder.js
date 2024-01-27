import { readFile } from "../file";
import { htmlPage } from "./dsl";
import { getTagLink, findTags } from "./parseDSL";

// is the link string that we have internal?
const isInternalLink = (l) => {
  const isExternal = l.includes("http://") || l.includes("https://") || l.startsWith("#");

  return !isExternal;
};

// Convert a link string to a legitimate file path on disk.
// probably requires more arguments
const linkStringToFile = (l, settings) => {
  const { sourceDir } = settings;
  return sourceDir.concat(l.toString());
};

// Represents an HTML AST that may not be associated with a file
class HtmlPage {
  pageStructure = null;
  currentBuildSettings = null;

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
    return findTags(this.pageStructure, ["a", "href", "img", "script"])
      .map(getTagLink)
      .filter(isInternalLink)
      .map(f => linkStringToFile(f, settings))
      .map(readFile);
  }

  toString() {
    return htmlPage(this.pageStructure);
  }
}

export default HtmlPage;
