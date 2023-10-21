import { exec } from './cmd';
import { Path } from './path';

// a git repository is a directory with a .git subdirectory
class Repo {
  // the full path to the git repository
  path = null;

  // the remote URL of the git repository
  // optional?
  remoteUrl = null;

  constructor(sourcePath, remotePath) {
    const path = new Path(dir);
    this.path = path;
  }

  // get the link to a particular file in git history
  historyLink(longHash, filePath) {
    return `${this.remoteUrl}/blob/${longHash}/${filePath}`;
  }

  // run a command in this git repository
  async runCmd(command) {
    return exec(command, { cwd: this.path.toString() });
  }

  // Get the full log of a file at the provided path
  async log(file) {
    const filePath = path.join(this.path.toString(), file);
    const command = `git log --all --full-history --pretty="format:%h %H %ad" --date default --date=format:'%Y-%m-%d' ${filePath}`;
    try {
      const { stdout } = await exec(command);
      if (stdout) {
        return stdout.split('\n').map((line) => {
          const [shortHash, longHash, commitDate] = line.split(' ');
          return {
            shortHash,
            longHash,
            commitDate,
            file: file,
          };
        });
      } else {
        throw new Error(`git log command failed on path ${file}`);
      }
    } catch (error) {
      throw error;
    }
  }

  // get the last log of a file at the provided path
  async lastLog(file) {
    const filePath = path.join(sourceDir, file);
    const command = `git log -1 --full-history --pretty="format:%h %H %ad %ct" --date default --date=format:'%Y-%m-%d' ${filePath}`;
    try {
      const { stdout } = await exec(command);
      if (stdout) {
        const [line] = stdout.split('\n');
        const [shortHash, longHash, commitDate, timestamp] = line.split(' ');
        return {
          shortHash,
          longHash,
          commitDate,
          timestamp: parseInt(timestamp, 10),
          file: file,
        };
      } else {
        throw new Error(`git log command failed on path ${file}`);
      }
    } catch (error) {
      throw error;
    }
  }

  // get the last timestamp of a file at the provided path
  async lastTimestamp(path) {
    const filePath = path ? path : '';
    const command = `git log -1 --pretty=format:%ct --follow -- ${filePath}`;
    try {
      const { stdout } = await exec(command, { cwd: sourceDir });
      return parseInt(stdout, 10);
    } catch (error) {
      throw error;
    }
  }

  historyLink(longHash, filePath) {
    return historyLink(longHash, filePath);
  }

  async checkout(branchName) {
    return this.runCmd(`git checkout ${branchName}`);
  }

  async addAll() {
    await this.runCmd('git add .');
  }

  async commit(message = 'robot commit') {
    await this.runCmd(`git -c commit.gpgsign=false commit -m "${message}"`);
  }

  async push() {
    await this.runCmd('git push');
  }

  async removeUntracked() {
    await this.runCmd('git clean -fxd');
  }

  async currentBranch() {
    const { stdout } = await this.runCmd('git branch --show-current');
    return stdout.trim();
  }

  async status() {
    const { stdout } = await this.runCmd('git status');
    console.log(stdout);
  }

  async stash() {
    await this.runCmd('git stash');
  }

  async stashPop() {
    await this.runCmd('git stash pop');
  }
}

export { Repo };
