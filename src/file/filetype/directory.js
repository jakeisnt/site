import { File } from '../classes';

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

  get isDirectory() {
    return true;
  }
}

export default Directory;
