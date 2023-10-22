import File from './file';

// A BinaryFile is a file that we can't read or write as a string (like an image)
class BinaryFile extends File {
  // the binary contents of the file
  asBinary = null;

  read() {
    this.asBinary = this.path.readBinary();
    return this;
  }

  write() {
    this.path.writeBinary(this.asBinary);
    return this;
  }
}

export default BinaryFile;
