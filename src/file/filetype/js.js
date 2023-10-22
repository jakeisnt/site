import { TextFile } from '../classes';

class JavascriptFile extends TextFile {
  static filetypes = ['js', 'jsx', 'ts', 'tsx'];

  require() {
    return require(this.path.toString());
  }
}

export default JavascriptFile;
