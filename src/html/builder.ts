import { readFile } from "../file";
import { htmlPage } from "./dsl";
import { getTagLink, findTags } from "./parseDSL";
import type { PageSettings } from "../types/site";
import type { PageSyntax, HtmlNode } from "../types/html";
import { isArray } from "../utils/array";
import { File } from "../file/classes";

/**
 * Is the link string that we are provided internal?
 * @param l the link to reference
 * @param settings page settings configuration
 * @returns
 */
const isInternalLink = (l: string, { rootUrl }: PageSettings) => {
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

/**
 * Convert a link string to a legitimate file path on disk.
 *
 * @param l the link to convert
 * @param settings page settings to carry
 */
const linkStringToFile = (l: string, settings: PageSettings) => {
  const { sourceDir, rootUrl } = settings;

  // remove the leading rootUrl from the link if it exists
  const linkWithoutRoot = l
    .replace(rootUrl, "")
    .replace("http://", "")
    .replace("https://", "");

  // path the now-local url to the source dir
  return sourceDir.toString().concat(linkWithoutRoot);
};

/**
 * An HTML AST builder.
 * May or may not be assocaited with a file.
 */
class HtmlPage {
  private pageStructure: PageSyntax;
  private currentBuildSettings: PageSettings;
  private cachedDependencies: File[] | undefined;

  constructor(pageSyntax: PageSyntax, settings: PageSettings) {
    this.pageStructure = pageSyntax;
    this.currentBuildSettings = settings;
  }

  static create(pageSyntax: PageSyntax, settings: PageSettings) {
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
        "img",
        "script",
        "link",
      ])
        .map((n: HtmlNode) => {
          if (isArray(n)) {
            return getTagLink(n);
          }
          getTagLink;
        })
        .filter((v) => v)
        .filter((f) => f && isInternalLink(f, settings))
        .map((f) => f && linkStringToFile(f, settings))
        .map((f) => (f ? readFile(f, settings) : undefined))
        .filter((f) => !!f) as File[];
    }

    return this.cachedDependencies;
  }

  toString() {
    return htmlPage(this.pageStructure);
  }
}

export default HtmlPage;
