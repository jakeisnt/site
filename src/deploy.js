const { file, const, path, home, git, main } = require('./your_module'); // Assuming you have these modules

async function commitFolderTo({ repo, deploymentDir, branch }) {
  const currentBranch = git.currentBranch(repo);
  const tmpDir = "/tmp/jake-site-deploy";

  console.log("saving current changes");
  git.addAll(repo);
  git.stash(repo);

  console.log("copying deployment to tmp dir");
  git.checkout(repo, branch);
  file.move(deploymentDir, tmpDir, repo);
  git.status(repo);

  console.log("removing all untracked files");
  git.removeUntracked(repo);
  git.status(repo);

  console.log("moving tmp dir contents to root");

  file.copyDir(tmpDir, deploymentDir, repo);
  // Note: Copying files within a directory in JavaScript may require additional code to list and copy each file individually.
  git.status(repo);

  console.log("pushing build");
  console.log("we are on branch " + git.currentBranch(repo));
  git.addAll(repo);
  git.commit(repo);
  git.push(repo);
  git.status(repo);

  console.log("restoring working branch");
  git.checkout(repo, currentBranch);
  git.status(repo);

  console.log("removing untracked");
  git.removeUntracked(repo);
  git.status(repo);

  console.log("removing deployment dir");
  file.removeDir(deploymentDir, repo);
  console.log("moving tmp dir to deployment dir");
  file.move(tmpDir, deploymentDir, repo);

  git.stashPop(repo);
}

async function mainFunction() {
  const currentRepo = "/home/jake/site";
  const deploymentBranch = const.deploymentBranch;
  const targetDir = const.targetDir;
  await main.mainFunction();
  await commitFolderTo({ repo: currentRepo, branch: deploymentBranch, deploymentDir: targetDir });
}
