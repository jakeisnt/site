import { TextFile } from '../classes';

// a CSS file is a text file
class CSSFile extends TextFile {
  static filetypes = ['css'];
}

export default CSSFile;
