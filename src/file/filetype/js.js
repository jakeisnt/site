import { SourceFile } from '../classes';

class JavascriptFile extends SourceFile {
  static filetypes = ['js', 'jsx', 'ts', 'tsx'];

  require() {
    return require(this.path.toString());
  }
}

export default JavascriptFile;
