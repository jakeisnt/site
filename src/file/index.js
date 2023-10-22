// this file should be a standard interface for interacting with files.
//
// you should be able to create a file with a path string
// and the specific file class should handle the rest.

// import all the files from the 'filetype' directory
// and associate them with their filetype names


const filetypeNames = fs.readdirSync(__dirname + '/filetype');

console.log(filetypeNames);

const filetypes = filetypeNames.map((filetypeName) => {
  const fileClass = require('./filetype/' + filetypeName);
  return fileClass;
});


// read a file from disk and return the corresponding File
const readFile = (path) => {
  // get the file extension
  // match the file extension to the class
  // create a new instance of the class
  // read the file in as a string
}

export {
  readFile
};
