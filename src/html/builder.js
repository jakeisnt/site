import { htmlPage } from "./dsl";

// Represents an HTML AST that may not be associated with a file
class HtmlPage {
  pageStructure = null;
  dependencies = [];

  constructor(pageSyntax) {
    // TODO: Find the dependencies in the page structure somehow.
    // They can also be passed manually.
    // Not sure which is better for me at this stage!

    this.dependencies = [];
    this.pageStructure = pageSyntax;
  }

  static create(pageSyntax) {
    return new this(pageSyntax);
  }

  toString() {
    return htmlPage(this.pageStructure);
  }
}

export default HtmlPage;
