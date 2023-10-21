// this file manages deployment to an external service
// right now its just github pages lol

import { Repo } from './git';
import { buildWebsite } from './build';

async function commitFolderToBranch({ repo, folderToCommit, targetBranch }) {
  const tmpDir = "/tmp/jake-site-deploy";

  const currentBranch = await repo.currentBranch();

  console.log("saving current changes");
  repo.addAll();
  repo.stash();

  console.log("copying deployment to tmp dir");
  repo.checkout(targetBranch);
  // TODO file.move(folderToCommit, tmpDir, repo);
  repo.status();

  console.log("removing all untracked files");
  repo.removeUntracked();
  repo.status();

  console.log("moving tmp dir contents to root");
  // TODO: file.copyDir(tmpDir, folderToCommit, repo);
  repo.status();

  console.log("pushing build");
  console.log("we are on branch " + git.currentBranch(repo));
  repo.addAll();
  repo.commit();
  repo.push();
  repo.status();

  console.log("restoring working branch");
  repo.checkout(currentBranch);
  repo.status();

  console.log("removing untracked");
  repo.removeUntracked();
  repo.status();

  console.log("removing deployment dir");
  // TODO: file.removeDir(folderToCommit, repo);
  console.log("moving tmp dir to deployment dir");
  // TODO: file.move(tmpDir, folderToCommit, repo);

  repo.stashPop();
}

async function deploy({ currentRepo, deploymentBranch, targetDir }) {
  await buildWebsite();
  await commitFolderToBranch({ repo: currentRepo, branch: deploymentBranch, folderToCommit: targetDir });
}

export { deploy }
