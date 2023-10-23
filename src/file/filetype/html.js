import { SourceFile } from '../classes';
import { readFile } from '../index';

class HTMLFile extends SourceFile {
  static filetypes = ['html', 'htm'];

  static create(filePath) {
    // if we have the html, just return it
    if (filePath.exists()) {
      return new HTMLFile(filePath);
    }

    // otherwise, try to get the non-html version of the file
    const path = filePath.toString().replace('.html', '');
    const sourceFile = readFile(path);

    // because the new setter doesn't mutate the object,
    // it should not affect the object's text accessor, right?
    return {
      ...sourceFile,
      // the text of a wrapped html file is the htmlified file
      get text() {
        return sourceFile.asHtml();
      },
      get mimeType() {
        return 'text/html';
      }
    };
  }
}

export default HTMLFile;
