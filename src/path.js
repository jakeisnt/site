const path = require('path');

function replaceExtension(filePath, extension) {
  return filePath.replace(/\.\S+$/, `.${extension}`);
}

function swapext(filePath, extension) {
  return replaceExtension(filePath, extension);
}

function sourceToTarget(filePath, sourceDir, targetDir) {
  return filePath.replace(new RegExp(sourceDir), targetDir);
}

function folder(filePath) {
  const parts = filePath.split('/');
  return parts.slice(0, -1).join('/');
}

function removePrefix(filePath, prefix) {
  return filePath.replace(new RegExp(`^${prefix}`), '');
}

function split(filePath) {
  // Splitting on the first '/' gives us a leading empty string that we drop
  return filePath.split('/').slice(1);
}

function complete(pathList, prefix) {
  return pathList.map((p) => path.join(prefix, p));
}

module.exports = {
  replaceExtension,
  swapext,
  sourceToTarget,
  folder,
  removePrefix,
  split,
  complete,
};
