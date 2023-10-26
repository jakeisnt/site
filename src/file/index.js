import fs from 'fs';
import { Path } from 'utils/path';
import Directory from './filetype/directory';
import TextFile from 'file/classes/text';


// this file should be a standard interface for interacting with files.
//
// you should be able to create a file with a path string
// and the specific file class should handle the rest.

// import all the files from the 'filetype' directory
// and associate them with their filetype names


var filetypeMap = null;

// obtain a map of file to filetype
const getFiletypeMap = () => {
  // bootstrap the process; we know we have a directory
  const dir = new Directory(__dirname + '/filetype/');

  const newFiletypeMap = {};

  // problem: to bootstrap the process, we need to know what class
  // a file is before we can create it. but we need to create it
  dir.contents({ assumeJSFile: true }).map((file) => {
    // because we have a js file, we know we can require it
    const fileClass = file.require();
    return fileClass.default;
  }).forEach(fileClass => {
    if (!fileClass.filetypes) {
      throw new Error(`Filetype ${fileClass.name} does not have a 'filetypes' property`);
    }

    fileClass.filetypes.forEach(fileType => {
      if (newFiletypeMap[fileType]) {
        throw new Error(`Filetype ${fileType} already exists`);
      }

      newFiletypeMap[fileType] = fileClass;
    });
  });

  return newFiletypeMap;
}

// given the source path of a file, return the appropriate file class
const readFile = (incomingPath) => {
  console.log(`Reading file at ${incomingPath.toString()}`);
  if (!filetypeMap) {
    filetypeMap = getFiletypeMap();
  }

  // get the file extension
  const path = Path.create(incomingPath);
  const extension = path.extension;

  if (!(extension in filetypeMap)) {
    return TextFile.create(incomingPath);
    // console.log(`We do not currently support files with extension ${extension}`);
    // throw new Error(`We do not currently support files with extension ${extension}`);
  }

  const FiletypeClass = filetypeMap[extension];
  return FiletypeClass.create(path);
}

export { readFile };
