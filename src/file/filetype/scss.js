import { SourceFile } from '../classes';
import { exec } from '../../utils/cmd';

class SCSSFile extends SourceFile {
  static filetypes = ['scss', 'sass'];

  // use  'sass' to write this file as css
  write() {
    const sourcePath = this.path;
    exec(`sass ${sourcePath} ${targetPath}`);
  }
}

export default SCSSFile;
