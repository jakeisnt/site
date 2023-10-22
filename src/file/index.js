import fs from 'fs';
import { Path } from '../utils/path';
import Directory from './filetype/directory';

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

  // problem: to bootstrap the process, we need to know what class
  // a file is before we can create it. but we need to create it
  const mapping = dir.contents({ assumeJSFile: true }).map((file) => {
    // because we have a js file, we know we can require it
    const fileClass = file.require();
    console.log('Loaded file', fileClass.default);
    return fileClass;
  });
}

console.log(getFiletypeMap());


// given the source path of a file, return the appropriate file class
const readFile = (path) => {
  if (!filetypeMap) {
    filetypeMap = getFiletypeMap();
  }
  // get the file extension
  // match the file extension to the class
  // create a new instance of the class
  // read the file in as a string
}

export {
  readFile
};
