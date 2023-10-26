import RepoCommit from './commit';

// A file we know to be in a git repo
class RepoFile {
  // the path to the file
  path = null;

  // the repo this file is in
  repo = null;

  constructor(repo, path) {
    this.repo = repo;
    this.path = path;
  }

  static create(repo, path) {
    const file = new RepoFile(repo, path);
    return file;
  }

  // get the link to this file in git history at a particular commit
  historyLink(longHash) {
    return this.repo.historyLink(longHash, this.path);
  }

  // get the last commit that cared about this file
  get lastLog() {
    const command = `git log -1 --full-history --pretty="format:%h %H %ad %ct" --date default --date=format:'%Y-%m-%d' ${this.path.toString()}`;
    try {
      const stdout = this.repo.runCmd(command);
      if (stdout) {
        const [line] = stdout.split('\n');
        const [shortHash, longHash, commitDate, timestamp] = line.split(' ');
        return RepoCommit.create({
          shortHash,
          longHash,
          commitDate,
          timestamp: parseInt(timestamp, 10),
        });
      } else {
        throw new Error(`git log command failed on path ${this.path.toString()}`);
      }
    } catch (error) {
      throw error;
    }
  }

  // Get the full log of changes to this file
  get log() {
    const command = `git log --all --full-history --pretty="format:%h %H %ad" --date default --date=format:'%Y-%m-%d' ${this.path.toString()}`;
    try {
      const stdout = this.repo.runCmd(command);
      if (stdout) {
        return stdout.split('\n').map((line) => {
          const [shortHash, longHash, commitDate] = line.split(' ');
          return RepoCommit.create({
            shortHash,
            longHash,
            commitDate,
          });
        });
      } else {
        throw new Error(`git log command failed on path ${this.path.toString()}`);
      }
    } catch (error) {
      throw error;
    }
  }

  // get the last timestamp of this file
  get lastTimestamp() {
    const command = `git log -1 --pretty=format:%ct --follow -- ${this.path.toString()}`;
    try {
      const stdout = this.repo.runCmd(command);
      return parseInt(stdout, 10);
    } catch (error) {
      throw error;
    }
  }
}

export default RepoFile;
