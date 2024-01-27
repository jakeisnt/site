import { readFile } from "../file";
import { htmlPage } from "./dsl";
import { getTagLink, findTags } from "./parseDSL";

// is the link string that we have internal?
const isInternalLink = (l) => true;

// Convert a link string to a legitimate file path on disk.
// probably requires more arguments
const linkStringToFile = (l) => "TODO";

// Represents an HTML AST that may not be associated with a file
class HtmlPage {
  pageStructure = null;

  constructor(pageSyntax) {
    // TODO: Find the dependencies in the page structure somehow.
    // They can also be passed manually.
    // Not sure which is better for me at this stage!

    this.pageStructure = pageSyntax;
  }

  static create(pageSyntax) {
    return new this(pageSyntax);
  }

  // Find all of the dependencies in the page.
  // We define a 'dependency' as any internal link.
  // For example -- src/index.css, src/file.html, etc.
  // Produces these dependencies as Files.
  dependencies(settings, filesSeenSoFar) {
    return findTags(this.pageStructure, ["a", "href", "img", "script"])
      .map(getTagLink)
      .filter(isInternalLink)
      .map(linkStringToFile)
      .map(readFile);
  }

  toString() {
    return htmlPage(this.pageStructure);
  }
}

export default HtmlPage;
