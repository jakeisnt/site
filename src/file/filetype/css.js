import TextFile from '../classes';

// a CSS file is a text file
class CSSFile extends TextFile {
  type = 'css';
}

export default CSSFile;
