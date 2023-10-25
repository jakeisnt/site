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
    const sourceFile = prevFile.clone(filePath);

    Object.defineProperty(sourceFile, 'text', {
      get() {
        console.log('do we have the prev file?', prevFile, prevFile.asHtml);
        return prevFile.asHtml();
      },
    });

    Object.defineProperty(sourceFile, 'mimeType', {
      get() {
        return 'text/html';
      },
    });

    return sourceFile;
  }
}

export default HTMLFile;
