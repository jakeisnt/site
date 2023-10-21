import fs from 'fs';
import path from 'path';
import fileType from './filetype/main'; // Import the fileType module
import { website } from './constants';

// The commit on which the file was last built
const lastCommitTimestamp = fs.existsSync(const.lastModifiedFile)
  ? parseInt(fs.readFileSync(const.lastModifiedFile).toString().trim())
  : 0;

console.log('Last build was at', lastCommitTimestamp);

function sortFilesByKey(files, key) {
  if (!key) {
    return files;
  }
  return files.sort((a, b) => key(b) - key(a));
}

function fileIsNew(file, config) {
  return config.forceRebuild || file.lastLog.timestamp > lastCommitTimestamp;
}

function recordLastTimestamp(sourceDir) {
  const lastTimestamp = git.lastTimestamp(sourceDir);
  fs.writeFileSync(const.lastModifiedFile, lastTimestamp.toString());
}

function getDirFiles(sourceDir, targetDir, config) {
  const filesToShow = fs.readdirSync(sourceDir);
  const files = filesToShow.map((fileX) =>
    fileType.info(fileX, sourceDir, targetDir, config.websiteTarget)
  );
  const sortedFiles = sortFilesByKey(files, config.sortBy);
  return sortedFiles;
}

function compileFile(fileObj, files, fileListIdx, _) {
  console.log('Compiling file:', fileObj.sourcePath);
  return fileType.withContents(fileObj, files, fileListIdx);
}

function compileDirectory(dirInfo, adjacentFiles, adjacentIdx, config) {
  console.log('Compiling dir:', dirInfo.sourcePath);
  const preFiles = getDirFiles(
    dirInfo.sourcePath,
    dirInfo.targetPath,
    config
  );
  const compiledFiles = preFiles.map((file, fileListIdx) =>
    compileUnit(file, preFiles, fileListIdx, config)
  );
  return fileType.withContents(
    // TODO: with-contents as implemented does not make sense with dirs?
    // Dirs should probably be special-cased more aggressively
    Object.assign({}, dirInfo, { children: compiledFiles }),
    compiledFiles,
    null
  );
}

function compileUnit(fileInfoObj, adjacentFiles, adjacentIdx, config) {
  if (fileIsNew(fileInfoObj, config)) {
    return (fileInfoObj.isDir
      ? compileDirectory
      : compileFile)(fileInfoObj, adjacentFiles, adjacentIdx, config);
  } else {
    console.log('  Skipping:', fileInfoObj.sourcePath);
    return fileInfoObj;
  }
}

function compileWikiPath({ folder, websiteTarget }, sourceDir, targetDir) {
  const folderConfig = config.folder;
  const sourcePath = path.join(sourceDir, folderConfig);
  const targetPath = path.join(targetDir, folderConfig);
  const dirInfo = fileType.info(
    sourcePath,
    sourceDir,
    targetDir,
    websiteTarget
  );

  console.log(`Compiling files from '${sourcePath}' to '${targetPath}'`);
  return compileUnit(dirInfo, [], null, config);
}

function compileHomePage(targetDir) {
  console.log('Writing home page');
  home.toDisk(targetDir);
}


// compile the full website
function main() {
  // pull in website config
  const { target: targetDir, source } = website;

  // for each website source:
  const compiledSite = website.sources.map(({ dir, paths }) =>
    // for each path within that source:
    paths.map((pathConfig) => {
      // compile all of the files at that path.
      fileType.toDisk(
        compileWikiPath(
          { ...pathConfig, websiteTarget: targetDir },
          source.dir,
          targetDir
        )
      );
    });
  );

  // now that we have all of that info, compile the home page.
  compileHomePage(targetDir);

  // when should the files be written to disk?
  //
  // thinking about a more OO approach:
  // File is a class that has methods

  // re/record the build timestamp.
  // TODO: these should be recorded per-source instead.
  recordLastTimestamp(targetDir);
}

// main();