import { Path } from "utils/path";
import File from "./classes/file";

import Directory from "./filetype/directory";
import TextFile from "file/classes/text";
import JavascriptFile from "./filetype/js";
import type { PageSettings } from "../types/site";

/*
 * A standard interface for interacting with files.
 *
 * Create a file with the provided path string
 * and the specific file class should handle the rest.
 *
 * Import all the files from the 'filetype' directory
 * and associate them with their filetype names.
 */

// key: a file extension
// value: a list of files that can compile to that file extension
// by invoking a function with that extension's name
const compileMap: { [key: string]: (typeof File)[] } = {};

type FiletypeMap = { [key: string]: typeof File };

// The algorithm has to work like so:
// 1. try to create() a file.
// 2. if success, return that file.
// 3. if the file can't be found,
//    collect a list of all file types that include this file's extension
//    in their `targets` array.
// 4. For each of those files,
//    swap our current file path with the extension of that file type and
//    attempt to create() it.
//    notice that this is a recursive call to the function: goes to (1).
//
//    we take the first file that fits. there is no specified precedence.
//    notably, this means it is not allowed to have two files with the same name but different extensions
//    that support the same compilation target,
//    in a directory, because those will conflict and one will take precedense

// If we don't have the JS file, try grabbing the TS file.

// Really, though, this file should not have to know what can compile to it.
// We need to register that in the source file somehow.

let filetypeMap: FiletypeMap;

// obtain a map of file to filetype
const getFiletypeMap = (cfg: PageSettings) => {
  // bootstrap the process; we know we have a directory
  const dir = new Directory(Path.create(__dirname + "/filetype/"), cfg);

  const newFiletypeMap: FiletypeMap = {};

  // Problem: to bootstrap the process, we need to know what class
  // a file is before we can create it. but we need to create it
  dir
    .contents(cfg, { omitNonJSFiles: true })
    .map((file: File) => {
      // because we have a js file, we know we can require it
      const fileClass = (file as JavascriptFile).require();
      // default to using the raw 'fileClass' if there is no default export (?)
      return fileClass?.default ?? fileClass;
    })
    .forEach((fileClass) => {
      if (!fileClass.filetypes) {
        throw new Error(
          `Filetype ${fileClass.name} does not have a 'filetypes' property`
        );
      }

      fileClass.filetypes.forEach((fileType: string) => {
        if (newFiletypeMap[fileType]) {
          throw new Error(`Filetype ${fileType} already exists`);
        }

        newFiletypeMap[fileType] = fileClass;
      });

      fileClass.targets?.forEach((target: string) => {
        compileMap[target] = (compileMap?.[target] ?? []).concat(fileClass);
      });
    });

  return newFiletypeMap;
};

const fileCache: { [key: string]: File } = {};

/**
 * Given the source path of a file, return the appropriate file class.
 * @param {string} incomingPath - The source path of the file.
 * @param {Object} options - Additional options.
 * @returns {Object} The appropriate file class.
 */
const readFile = (path: Path, options: PageSettings): File => {
  if (!filetypeMap) {
    filetypeMap = getFiletypeMap(options);
  }

  const { sourceDir, fallbackSourceDir } = options;

  // If the path doesn't exist, try it against a fallback
  if (!path.exists() && sourceDir && fallbackSourceDir) {
    path = path.relativeTo(sourceDir, fallbackSourceDir);
  }

  const extension = path.extension;

  if (!fileCache[path.toString()]) {
    if (!extension || !(extension in filetypeMap)) {
      console.log(
        `We don't have a filetype mapping for files with extension ${extension}. Assuming plaintext for file at path '${path.toString()}'.`
      );

      fileCache[path.toString()] = TextFile.create(path, options);
    } else {
      const FiletypeClass = filetypeMap[extension];
      fileCache[path.toString()] = FiletypeClass.create(path, options);
    }
  }

  return fileCache[path.toString()];
};

export { readFile };
