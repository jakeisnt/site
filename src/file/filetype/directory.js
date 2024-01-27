import { File } from "file/classes";
import JSFile from "./js.js";
import { readFile } from "file";
import { header, component } from "html";
import HtmlPage from "../../html/builder.js";

const readJSFile = (path) => {
  return new JSFile(path);
};

const folderIndexPageTable = ({ files, rootUrl, sourceDir }) => {
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
              { href: childFile.htmlUrl({ rootUrl, sourceDir }) },
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

const directoryToHtml = (dir, { files, rootUrl, siteName, sourceDir }) => {
  const title = dir.name;

  return [
    "html",
    header({ title, siteName, rootUrl }),
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

// a directory is a file that contains other files
class Directory extends File {
  static filetypes = ["dir"];

  // recursively fetch and flatten the file tree
  // the result should contain no directories
  tree() {
    const myContents = this.contents();
    const childFiles = [];

    myContents.forEach((file) => {
      if (file.isDirectory) {
        childFiles.push(...file.tree());
      } else {
        childFiles.push(file);
      }
    });

    return childFiles;
  }

  // get all the files in the immediate dir
  // we don't treat this as a property and don't cache it
  // because it is very possible for the files in the dir to change

  // the 'assumeJSFile' flag exists to bootstrap the setup:
  // the readFile function knows how to dispatch because it reads the files in this directory,
  // but it doesn't know what kind of files they are yet - so we force JS.
  contents({ omitNonJSFiles } = { omitNonJSFiles: false }) {
    const readFileWithType = omitNonJSFiles ? readJSFile : readFile;

    return this.path.readDirectory().map((childPath) => {
      if (omitNonJSFiles && childPath.extension !== "js") {
        return null;
      } else {
        return readFileWithType(childPath);
      }
    }).filter(file => file);
  }

  // given a file path relative to this directory,
  // find the relevant source file
  findFile(relativePath) {
    const path = this.path.join(relativePath);

    try {
      return readFile(path);
    } catch (e) {
      return null;
    }
  }

  write(config) {
    const { sourceDir, targetDir } = config;

    // first, make sure the corresponding directory exists.
    // this is e.g. '/site/docs/' and mkdir /site/docs/
    const targetPath = this.path.relativeTo(sourceDir, targetDir);
    // make sure the target html path exists, too.
    // creating a folder both creates the target dir and an index page at the target.
    const targetHtmlPath = targetPath.join("/index.html");

    // we need to write /site/docs.html
    // this should implicitly create the parent directory if it doesn't exist!
    targetHtmlPath.writeString(this.serve(config).contents);
  }
  
  // the dependencies of a directory are all of the files that it contains
  dependencies() {
    return this.contents();
  }

  get isDirectory() {
    return true;
  }

  asHtml({ siteName, rootUrl, sourceDir }) {
    const files = this.contents();
    const page = directoryToHtml(this, { files, siteName, rootUrl, sourceDir });

    return HtmlPage.create(page);
  }

  serve(args) {
    return { contents: this.asHtml(args).toString(), mimeType: this.mimeType };
  }

  // as of now, we only use folders for their html
  get mimeType() {
    return "text/html";
  }
}

export default Directory;
