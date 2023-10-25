import { SourceFile } from '../classes';
import { readFile } from '../index';
import { cloneClassInstance } from '../../utils/class';

class HTMLFile extends SourceFile {
  static filetypes = ['html', 'htm'];

  static create(filePath) {
    // if we have the html, just return it
    if (filePath.exists()) {
      return new HTMLFile(filePath);
    }

    // otherwise, try to get the non-html version of the file
    const path = filePath.toString().replace('.html', '');

    const prevFile = readFile(path);
    console.log('copying prev file', prevFile);
    const sourceFile = cloneClassInstance(prevFile);

    console.log('source file', sourceFile);


    try {
      Object.defineProperty(sourceFile, 'text', {
        get() {
          return prevFile.asHTML();
        },
        writable: true,
        readable: true,
        enumerable: true,
      });

      Object.defineProperty(sourceFile, 'mimeType', {
        get() {
          return 'text/html';
        },
        writable: true,
        readable: true,
        enumerable: true,
      });
} catch (e) {
  console.log(e);
}

    return sourceFile;
  }
}

export default HTMLFile;
