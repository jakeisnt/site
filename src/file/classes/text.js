import File from './file';

// A text file is a file that can be read as a utf-8 string
class TextFile extends File {
  // the string contents of the file
  asString = null;

  read() {
    this.asString = fs.readFileSync(this.path, 'utf8');
    return this;
  }

  write() {
    this.path.writeString(this.asString);
    return this;
  }

  get text() {
    if (!this.asString) {
      this.read();
    }

    return this.asString;
  }

  // when this file is requested, return the text
  async onRequest(callback) {
    return this.text;
  }
}

export default TextFile;
