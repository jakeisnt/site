import { SourceFile } from '../classes';
import * as ts from "typescript";
import { readFile } from '../index';

const tsToJs = (tsText) => {
  const options = { compilerOptions: { module: ts.ModuleKind.CommonJS }};
  return ts.transpileModule(tsText, options).outputText;
}

class JavascriptFile extends SourceFile {
  static filetypes = ['js'];

  require() {
    return require(this.path.toString());
  }

  // override the create behavior to create the parent
  static create(filePath) {
    if (filePath.exists()) {
      return new JavascriptFile(filePath);
    }

    // if this file doesn't exist, try making the typescript file.
    const tsPath = filePath.replaceExtension('ts');
    const newFile = readFile(tsPath);
    const sourceFile = newFile.clone();

    Object.defineProperty(sourceFile, 'text', {
      get() {
        return tsToJs(newFile.text);
      }
    });

    Object.defineProperty(sourceFile, 'mimeType', {
      get() {
        return 'text/javascript';
      }
    });

    return sourceFile;
  }
}

export default JavascriptFile;
