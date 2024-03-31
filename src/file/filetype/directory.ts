import { File } from "file/classes";
import JSFile from "./js.js";
import { readFile } from "file";
import { header, component } from "html";
import HtmlPage from "../../html/builder.js";
import { PageSettings } from "../../types/site.js";
import { PageSyntax } from "../../types/html.js";
import { Path } from "../../utils/path.js";

/**
 * Read a javascript file to string.
 */
const readJSFile = (path: Path) => {
  return new JSFile(path);
};

const folderIndexPageTable = ({ files, rootUrl, sourceDir }): PageSyntax => {
  return [
    "div",
    { class: "folder-index-page-table" },
    [
      "table",
      files.map((childFile) => {
        const lastLog = childFile.lastLog;
        return [
          "tr",
          ["td", { class: "file-hash-tr" }, lastLog?.shortHash],
          [
            "td",
            { class: "file-name-tr" },
            [
              "a",
              {
                href: childFile.htmlUrl({
                  rootUrl,
                  sourceDir,
                }),
              },
              childFile.name,
            ],
          ],
          ["td", { class: "file-type-tr" }, childFile.extension],
          [
            "td",
            {
              class: lastLog?.date ? "file-date-tr" : "file-date-untracked-tr",
            },
            lastLog?.date ?? "untracked",
          ],
        ];
      }),
    ],
  ];
};

const directoryToHtml = (
  dir: Directory,
  {
    files,
    rootUrl,
    siteName,
    sourceDir,
    resourcesDir,
    faviconsDir,
  }: PageSettings & { files: File[] }
): PageSyntax => {
  const title = dir.name;

  return [
    "html",
    header({ title, siteName, rootUrl, resourcesDir, faviconsDir }),
    [
      "body",
      component("Sidebar", { path: dir.path, title, sourceDir, rootUrl }),
      [
        "div",
        { class: "site-body" },
        [
          "main",
          folderIndexPageTable({ files, rootUrl, sourceDir }),
          component("ScrollUp"),
        ],
      ],
    ],
  ];
};

/**
 * Represents a directory, which is a file that contains other files.
 */
class Directory extends File {
  static filetypes = ["dir"];
  enumeratedContents = null;
  enumeratedDependencies = null;
  enumeratedHtml = null;

  // recursively fetch and flatten the file tree
  // the result should contain no directories
  tree() {
    const myContents = this.contents();
    const childFiles = [];

    myContents.forEach((file) => {
      if (file.isDirectory()) {
        childFiles.push(...file.tree());
      } else {
        childFiles.push(file);
      }
    });

    return childFiles;
  }

  /**
   * Get all the files in the immediate directory.
   * This is not treated as a property and is not cached due to the possibility of files changing.
   *
   * @param {boolean} omitNonJSFiles - Flag to bootstrap the setup. The 'readFile' function dispatches based on this flag to force JavaScript files.
   */
  contents(
    { omitNonJSFiles = false }: { omitNonJSFiles: boolean } = {
      omitNonJSFiles: false,
    }
  ): File[] {
    // special case for the js files: make sure they all exist.
    // don't cache this because we only want the default full dir cached.
    if (omitNonJSFiles) {
      return this.path
        .readDirectory()
        .map((childPath: Path) => {
          if (childPath.extension !== "js" && childPath.extension !== "ts") {
            return null;
          } else {
            return readJSFile(childPath);
          }
        })
        .filter((file: File | null) => file);
    }

    if (this.enumeratedContents) {
      return this.enumeratedContents;
    }

    const fileContents = this.path.readDirectory().map((v) => readFile(v));
    this.enumeratedContents = fileContents;
    return fileContents;
  }

  // given a file path relative to this directory,
  // find the relevant source file
  findFile(relativePath: Path) {
    const path = this.path.join(relativePath);

    try {
      return readFile(path);
    } catch (e) {
      return null;
    }
  }

  write(config: PageSettings) {
    const { sourceDir, targetDir } = config;

    // first, make sure the corresponding directory exists.
    // this is e.g. '/site/docs/' and mkdir /site/docs/
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    targetPath.make({ isDirectory: true });

    return this;
  }

  /**
   * Find all of the dependencies of a directory.
   *
   * The dependencies of a directory are all of the files that it contains,
   * but as html versions. this is a proxy for finding those links in the html
   * SHORTCUT: the dependencies of a directory in general are not html-ified.
   * That's a quick hack we use here to bootstrap building files.
   */
  dependencies(settings: PageSettings) {
    if (!this.enumeratedDependencies) {
      this.enumeratedDependencies = this.asHtml(settings).dependencies();
    }

    return this.enumeratedDependencies;
  }

  isDirectory() {
    return true;
  }

  asHtml(settings: PageSettings) {
    if (!this.enumeratedHtml) {
      const files = this.contents();
      const page = directoryToHtml(this, {
        files,
        ...settings,
      });

      this.enumeratedHtml = HtmlPage.create(page, settings);
    }

    return this.enumeratedHtml;
  }

  serve(args: PageSettings) {
    return { contents: this.asHtml(args).toString(), mimeType: this.mimeType };
  }

  // as of now, we only use folders for their html
  get mimeType() {
    return "text/html";
  }
}

export default Directory;
