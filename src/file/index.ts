import { Path } from "utils/path";

import logger from "utils/log";
import Directory from "./filetype/directory";
import TextFile from "file/classes/text";

// this file should be a standard interface for interacting with files.
//
// you should be able to create a file with a path string
// and the specific file class should handle the rest.

// import all the files from the 'filetype' directory
// and associate them with their filetype names

let filetypeMap;

// obtain a map of file to filetype
const getFiletypeMap = () => {
  // bootstrap the process; we know we have a directory
  const dir = new Directory(__dirname + "/filetype/");

  const newFiletypeMap = {};

  // problem: to bootstrap the process, we need to know what class
  // a file is before we can create it. but we need to create it
  dir
    .contents({ omitNonJSFiles: true })
    .map((file) => {
      // because we have a js file, we know we can require it
      const fileClass = file.require();
      // default to using the raw 'fileClass' if there is no default export (?)
      return fileClass?.default ?? fileClass;
    })
    .forEach((fileClass) => {
      if (!fileClass.filetypes) {
        throw new Error(
          `Filetype ${fileClass.name} does not have a 'filetypes' property`
        );
      }

      fileClass.filetypes.forEach((fileType) => {
        if (newFiletypeMap[fileType]) {
          throw new Error(`Filetype ${fileType} already exists`);
        }

        newFiletypeMap[fileType] = fileClass;
      });
    });

  return newFiletypeMap;
};

/**
 * Given the source path of a file, return the appropriate file class.
 * @param {string} incomingPath - The source path of the file.
 * @param {Object} options - Additional options.
 * @param {string} options.sourceDir - The source directory.
 * @param {string} options.fallbackSourceDir - The fallback source directory.
 * @returns {Object} The appropriate file class.
 */
const readFile = (
  incomingPath: string | Path,
  options?: { sourceDir: string; fallbackSourceDir?: string }
) => {
  logger.file(`Reading file at ${incomingPath.toString()}`);
  if (!filetypeMap) {
    filetypeMap = getFiletypeMap();
  }

  const { sourceDir, fallbackSourceDir } = options ?? {};

  // Get the file extension
  let path = Path.create(incomingPath);

  // If the path doesn't exist, try it against a fallback
  if (!path.exists() && sourceDir && fallbackSourceDir) {
    path = path.relativeTo(sourceDir, fallbackSourceDir);
  }

  const extension = path.extension;

  if (!(extension in filetypeMap)) {
    console.log(
      `We don't have a filetype mapping for files with extension ${extension}. Assuming plaintext for file at path '${path.toString()}'.`
    );

    return TextFile.create(path);
  }

  const FiletypeClass = filetypeMap[extension];
  return FiletypeClass.create(path);
};

export { readFile };
