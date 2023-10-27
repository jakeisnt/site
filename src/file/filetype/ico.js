import { BinaryFile } from 'file/classes';

// can follow!! https://github.com/lovell/sharp/issues/1118
//
// for html, you should inline the base64 image: https://stackoverflow.com/questions/13094257/display-an-ico-within-an-image-element-img
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
