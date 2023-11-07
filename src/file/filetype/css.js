import { SourceFile } from 'file/classes';
import SCSSFile from './scss';
import * as sass from 'sass';
const { pathToFileURL } = require('url');
import { readFile } from 'file';
import { sourceDir } from '../../constants';

const scssToCss = (scssText) => {
  const result = sass.compileString(scssText, {
    sourceMap: false,
    importers: [{
      findFileUrl(url) {
        // TODO hardcoded 'site' reference
        const nextUrl = new URL(url, pathToFileURL(`${sourceDir}/`));
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
    const sourceFile = newFile.clone();

    Object.defineProperty(sourceFile, 'text', {
      get() {
        return scssToCss(newFile.text);
      }
    });

    Object.defineProperty(sourceFile, 'mimeType', {
      get() {
        return 'text/css';
      }
    });

    return sourceFile;
  }
}

export default CSSFile;
