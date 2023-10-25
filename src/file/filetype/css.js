import { SourceFile } from '../classes';
import SCSSFile from './scss';
import * as sass from 'sass';
const { pathToFileURL } = require('url');
import { readFile } from '../index';
import { cloneClassInstance } from '../../utils/class';

const scssToCss = (scssText) => {
  const result = sass.compileString(scssText, {
    sourceMap: false,
    importers: [{
      findFileUrl(url) {
        // TODO hardcoded 'site' reference
        const nextUrl = new URL(url, pathToFileURL("/home/jake/site/"));
        return nextUrl;
      }
    }],

  });
  return result.css.toString();
};

// a CSS file is a text file
class CSSFile extends SourceFile {
  static filetypes = ['css'];

  // override the create behavior to create the parent
  static create(filePath) {
    if (filePath.exists()) {
      return new CSSFile(filePath);
    }

    // if this file doesn't exist, try making the scss file.
    const scssPath = filePath.replaceExtension('scss');

    const newFile = readFile(scssPath);
    const sourceFile = cloneClassInstance(newFile);

    Object.defineProperty(sourceFile, 'text', {
      get() {
        return scssToCss(newFile.text);
      }
    }, { writable: true,
         readable: true,
         enumerable: true,

       });

    Object.defineProperty(sourceFile, 'extension', {
      get() {
        return 'css';
      }
    }, { writable: true,

        readable: true,
        enumerable: true,
       });

    return sourceFile;
  }
}

export default CSSFile;
