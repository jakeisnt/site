import { SourceFile } from '../classes';

class ClojureFile extends SourceFile {
  static filetypes = ['clj', 'cljc', 'cljs', 'edn'];
}

export default ClojureFile;
