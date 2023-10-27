import { BinaryFile } from 'file/classes';

class IcoFile extends BinaryFile {
  static filetypes = ['ico'];

  serve() {
    console.log('--- SERVING FAVICON!!!!!!!!!!!!!');
    return this.path.readBinary();
  }

  asHtml() {
    console.log('reading favicon binary');
    console.log(this.path.readBinary());
    return `<html><img src="data:image/x-icon;base64,${this.path.readBinary().toString()}"></html>`;
  }
}

export default IcoFile;
