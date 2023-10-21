import { File } from '../classes';
import { header } from '../../html';

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

const directoryToHtml = (dir, { files }) {
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

// a directory is a file that contains other filesq
class Directory extends File {// get all the files in this dir, and those dirs, and those as a flat array
  tree() {
    const dir = this.path;
    const filePaths = fs.readdirSync(dir, { withFileTypes: true });
    const childFiles = [];

    filePaths.forEach((filePath) => {
      // TODO: how we get the path to the file depends on the name
      // okay, a file is a Dirent. we can find docs for this.
      const file = new File(path.join(dir, filePath));
      if (file.isDirectory) {
        result.push(...tree(filePath));
      } else {
        result.push(filePath);
      }
    });

    return childFiles;
  }

  // get all the files in the immediate dir
  contents() {
    const filesInDir = fs.readdirSync(this.path, { withFileTypes: true });
    return filesInDir.map((file) => {
      return new File(path.join(this.path, file));
    });
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
