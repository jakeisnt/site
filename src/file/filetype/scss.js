import { TextFile } from '../classes';
import { exec } from '../../utils/cmd';

class SCSSFile extends TextFile {
  type = 'scss';

  // use  'sass' to write this file as css
  write() {
    const sourcePath = this.path;
    exec(`sass ${sourcePath} ${targetPath}`);
  }

  // compile the file if it hasnt been, then serve from the target path
  onRequest() {
    throw new Error('scss on request isnt yet implemented');
  }
}

export default SCSSFile;
