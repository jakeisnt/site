// a single commit in a repository
// ask me questions about the specific commit
class RepoCommit {
  constructor({ shortHash, longHash, commitDate, timestamp, repo }) {
    this.shortHash = shortHash;
    this.longHash = longHash;
    this.commitDate = commitDate;
    this.timestamp = timestamp;
    this.repo = repo;
  }

  static create(args) {
    return new RepoCommit(args);
  }

  get date() {
    return this.commitDate;
  }

  // get the link associated with this repo commit at a given path
  // TODO not done yet
  historyLink(filePath) {
    if (!this.repo.remoteUrl) {
      throw new Error('Cannot create history link without a defined remote URL');
    }

    return `${this.repo.remoteUrl}/blob/${this.ongHash}/${filePath.toString()}`;
  }

}

export default RepoCommit;
