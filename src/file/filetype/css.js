import { SourceFile } from '../classes';
import SCSSFile from './scss';
import * as sass from 'sass';
const {pathToFileURL} = require('url');

const scssToCss = (scssText) => {
  const result = sass.compileString(scssText, {
    sourceMap: false,
    importers: [{
      findFileUrl(url) {
        // TODO hardcoded 'site' reference
        const nextUrl = new URL(url, pathToFileURL("/home/jake/site/"));
        console.log('next url', nextUrl.toString());
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

    // if this file doesn't exist,
    // try making the scss file
    const scssPath = filePath.replaceExtension('scss');

    // TODO: override the read behavior to compile the parent back to this file
    const newFile = SCSSFile.create(scssPath);

    // how does the new file
    return {
      ...newFile,
      get text() {
        // use sass to compile the scss file
        // at the scss file path
        return scssToCss(newFile.text);
      },
      get extension() {
        return 'css';
      }
    }
  }
}

export default CSSFile;
