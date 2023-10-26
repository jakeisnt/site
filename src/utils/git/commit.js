// a single commit in a repository
// ask me questions about the specific commit
class RepoCommit {
  constructor({ shortHash, longHash, commitDate, timestamp }) {
    this.shortHash = shortHash;
    this.longHash = longHash;
    this.commitDate = commitDate;
    this.timestamp = timestamp;
  }

  static create(args) {
    return new RepoCommit(args);
  }

  // get the link associated with this repo commit at a given path
  // TODO not done yet
  historyLink(path) {
    if (!this.remoteUrl) {
      throw new Error('Cannot create history link without a defined remote URL');
    }

    return `${this.remoteUrl}/blob/${longHash}/${filePath.toString()}`;
  }

}

export default RepoCommit;
