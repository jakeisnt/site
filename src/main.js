// entrypoint of the program; this is the cli

import { deploy } from './deploy';
import { deploymentBranch, targetDir, website } from './constants';
import { cli } from './utils/cli';
import { readFile } from './file';
import { singleFileServer } from './server';

const currentRepo = "/home/jake/site";

// function deployWebsite() {
//   deploy({ currentRepo, targetDir, deploymentBranch });
// }

// compile the full website
// function compileWebsite() {
//   // pull in website config
//   const { target: targetDir, source } = website;

//   // for each website source:
//   const compiledSite = website.sources.map(({ dir, paths }) =>
//     // for each path within that source:
//     paths.map((pathConfig) => {
//       // compile all of the files at that path.
//       fileType.toDisk(
//         compileWikiPath(
//           { ...pathConfig, websiteTarget: targetDir },
//           source.dir,
//
//         ));
//     }
//     ));

//   // now that we have all of that info, compile the home page.
//   compileHomePage(targetDir);

//   // when should the files be written to disk?
//   //
//   // thinking about a more OO approach:
//   // File is a class that has methods

//   // re/record the build timestamp.
//   // TODO: these should be recorded per-source instead.
//   recordLastTimestamp(targetDir);
// }

// Serve a file from a particular path.
  // Supports hot reloading.
const serve = (paths) => {
  singleFileServer(paths[0]);

  // const path = paths?.[0];
  // console.log('serving file at path: ', path);

  // const file = readFile(path);
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
