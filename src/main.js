// entrypoint of the program; this is the cli

import { deploy } from './deploy';
import { deploymentBranch, targetDir } from './constants';

const currentRepo = "/home/jake/site";

function deployWebsite() {
  deploy({ currentRepo, targetDir, deploymentBranch });
}
