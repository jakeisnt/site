import { File } from "file/classes";
import JSFile from "./js.js";
import { readFile } from "file";
import { header, component, html } from "html";

const readJSFile = (path) => {
  return new JSFile(path);
};

const folderIndexPageTable = ({ files, rootUrl, sourceDir }) => {
  return [
    "div",
    { class: "folder-index-page-table" },
    [
      "table",
      files.map((childFile) => [
        "tr",
        ["td", { class: "file-hash-tr" }, childFile.lastLog?.shortHash],
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
        ["td", { class: "file-date-tr" }, childFile.lastLog?.date],
      ]),
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
  contents({ assumeJSFile } = { assumeJSFile: false }) {
    const readFileWithType = assumeJSFile ? readJSFile : readFile;

    return this.path.readDirectory().map((childPath) => {
      return readFileWithType(childPath);
    });
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

  write() {
    // make target directory
    // generate html for this file
    throw new Error("write for directories has not been implemented yet");
  }

  get isDirectory() {
    return true;
  }

  asHtml({ siteName, rootUrl, sourceDir }) {
    const files = this.contents();
    return html(directoryToHtml(this, { files, siteName, rootUrl, sourceDir }));
  }

  serve(args) {
    return { contents: this.asHtml(args), mimeType: this.mimeType };
  }

  // as of now, we only use folders for their html
  get mimeType() {
    return "text/html";
  }
}

export default Directory;
