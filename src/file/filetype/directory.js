import { File } from '../classes';
import JSFile from './js.js';
import { readFile } from '../index';
import { header } from '../../html';

const readJSFile = (path) => {
  return new JSFile(path);
}

const folderIndexPageTable = (files) => {
  return ["div",
        { class: 'folder-index-page-table' },
        ["table",
         files.map((childFile) =>
           ["tr",
            ["td", { class: 'file-hash-tr' }, childFile.lastCommitLog.shortHash],
            ["td", { class: 'file-name-tr' }, childFile.name],
            ["td", { class: 'file-type-tr' }, childFile.extension],
            ["td", { class: 'file-date-tr' }, childFile.lastCommitLog.shortHash],
           ]
         )
        ]
       ];
}

const directoryToHtml = (dir, { files }) => {
  return [
    "html",
    header(),
    ["body",
     component("Sidebar", { file: dir, files }),
     ["div",
      { class: 'site-body' },
      ["main",
       folderIndexPageTable(files),
       component("ScrollUp", { file: dir, files })]]]];
}

// a directory is a file that contains other files
class Directory extends File {
  static filetypes = ['directory'];

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
    throw new Error('write for directories has not been implemented yet');
  }

  get isDirectory() {
    return true;
  }
}

export default Directory;
