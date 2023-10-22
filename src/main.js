// entrypoint of the program; this is the cli

import { deploy } from './deploy';
import { deploymentBranch, targetDir, website } from './constants';
import { cli } from './utils/cli';
import { readFile } from './file';
import { singleFileServer } from './server';

const currentRepo = "/home/jake/site";

// Serve a file from a particular path.
// Supports hot reloading.
const serve = (paths) => {
  if (!paths.length) {
    console.log("No file path specified. Not serving anything.");
    return;
  }

  singleFileServer(paths[0]);
}

const app = cli('site')
      .describe('compiles the website')
      .option('deploy').describe('deploy the website').action(() => console.log('deploy'))
      .option('build').describe('build the website').action(() => console.log('build'))
      .option('serve').describe('serve the website').action(serve);

function main() {
  const args = process.argv.slice(2);
  app.exec(args);
}

main();
