import { readFile } from "../file";
import { htmlPage } from "./dsl";
import type { PageSyntax, Dependency } from "../types/html";
import { File } from "../file/classes";
import type { PageSettings } from "../types/site";
import { Path } from "../utils/path";

/**
 * Is the link string that we are provided internal?
 * @param l the link to reference
 * @param settings page settings configuration
 * @returns
 */
const isInternalLink = (l: string, { url }: { url: string }) => {
  if (l.includes(url)) {
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
const linkStringToFile = (
  l: string,
  { url, sourceDir }: { url: string; sourceDir: Path }
) => {
  // remove the leading rootUrl from the link if it exists
  const linkWithoutRoot = l
    .replace(url, "")
    .replace("http://", "")
    .replace("https://", "");

  return sourceDir.join(`/${linkWithoutRoot}`);
};

const makeDependencies = (
  dependencies: Dependency[],
  settings: PageSettings
): File[] => {
  const res: File[] = [];

  dependencies.forEach((dep) => {
    if (!isInternalLink(dep.src, settings)) {
      return;
    }

    if (settings.targetDir.equals(dep.src)) {
      return;
    }

    const file = readFile(linkStringToFile(dep.src, settings), settings);
    if (!file) return;

    res.push(file);
  });

  return res;
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
      this.toString();
    }

    return this.cachedDependencies ?? [];
  }

  toString() {
    const { dependsOn, body } = htmlPage(
      this.pageStructure,
      this.currentBuildSettings
    );

    this.cachedDependencies = makeDependencies(
      dependsOn,
      this.currentBuildSettings
    );

    return body;
  }
}

export default HtmlPage;
