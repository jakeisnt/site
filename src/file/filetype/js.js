import { TextFile } from '../classes';

class JavascriptFile extends TextFile {
  require() {
    return require(this.path.toString());
  }
}

export default JavascriptFile;
