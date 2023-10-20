const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);

function getPath(file) {
  return (typeof file === 'string') ? file : file.getPath();
}

function tree(dir) {
  const files = fs.readdirSync(dir, { withFileTypes: true });
  const result = [];
  files.forEach((file) => {
    const filePath = path.join(dir, file.name);
    if (file.isDirectory()) {
      result.push(...tree(filePath));
    } else {
      result.push(filePath);
    }
  });
  return result;
}

function listDir(dir) {
  return fs.readdirSync(dir);
}

function read(filePath) {
  try {
    return fs.readFileSync(filePath, 'utf8');
  } catch (e) {
    console.error('File read from path not found:', filePath);
    console.error(e);
    return null;
  }
}

function readImage(filePath) {
  try {
    return fs.createReadStream(filePath);
  } catch (e) {
    console.error('File read from path not found:', filePath);
    console.error(e);
    return null;
  }
}

async function writeImage(inStream, outPath) {
  const outStream = fs.createWriteStream(outPath);
  inStream.pipe(outStream);
  await new Promise((resolve) => {
    outStream.on('close', resolve);
  });
}

function makeDirectory(dir) {
  fs.mkdirSync(dir);
}

function write(content, filePath) {
  fs.writeFileSync(filePath, content, 'utf8');
}

function title(filePath) {
  const parts = filePath.split('/');
  return parts[parts.length - 1];
}

function name(filePath) {
  const fileName = title(filePath);
  return fileName.split('.')[0];
}

function extension(filePath) {
  const fileName = title(filePath);
  const parts = fileName.split('.');
  return parts.length > 1 ? parts[1] : '';
}

function isDir(path) {
  return fs.statSync(path).isDirectory();
}

async function move(from, to, dir) {
  const { stdout, stderr } = await exec(`mv "${from}" "${to}"`, { cwd: dir });
  console.log(stdout);
  console.error(stderr);
}

async function copyDir(from, to, dir) {
  const { stdout, stderr } = await exec(`cp -r "${from}" "${to}"`, { cwd: dir });
  console.log(stdout);
  console.error(stderr);
}

async function copy(from, to, fromDir) {
  await copyDir(from, to, fromDir);
}

async function removeDir(path, inDir) {
  const { stdout, stderr } = await exec(`rm -r "${path}"`, { cwd: inDir });
  console.log(stdout);
  console.error(stderr);
}

function exists(filePath) {
  return fs.existsSync(filePath);
}

module.exports = {
  getPath,
  tree,
  listDir,
  read,
  readImage,
  writeImage,
  makeDirectory,
  write,
  title,
  name,
  extension,
  isDir,
  move,
  copyDir,
  copy,
  removeDir,
  exists,
};
