import { execSync } from '../cmd';
import RepoFile from './repofile';

// a git repository is a directory with a .git subdirectory
// This is basically a git porcelain that keeps the current repo
class Repo {
  // the full path to the repository
  // this is the directory that contains the .git subdirectory
  path = null;

  // the remote URL of the git repository
  // optional?
  remoteUrl = null;

  constructor(path) {
    this.path = path;
  }

  static create(sourcePath, remotePath) {
    const repo = new Repo(sourcePath, remotePath);
    return repo;
  }

  getFile(atPath) {
    if (!this.path.contains(atPath)) {
      throw new Error(`File ${atPath} is not in the repository`);
    }

    return RepoFile.create(this, atPath);
  }

  // run a command in this git repository
  runCmd(command) {
    const cmdResult = execSync(command, { cwd: this.path.toString() });
    return cmdResult.toString();
  }

  checkout(branchName) {
    return this.runCmd(`git checkout ${branchName}`);
  }

  addAll() {
    this.runCmd('git add .');
  }

  commit(message = 'robot commit') {
    this.runCmd(`git -c commit.gpgsign=false commit -m "${message}"`);
  }

  push() {
    this.runCmd('git push');
  }

  removeUntracked() {
    this.runCmd('git clean -fxd');
  }

  currentBranch() {
    const { stdout } = this.runCmd('git branch --show-current');
    return stdout.trim();
  }

  status() {
    const { stdout } = this.runCmd('git status');
    console.log(stdout);
  }

  stash() {
    this.runCmd('git stash');
  }

  stashPop() {
    this.runCmd('git stash pop');
  }
}

export default Repo;
