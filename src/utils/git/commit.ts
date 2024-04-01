import type { Repo } from ".";
// import type { Path } from "../path";

/**
 * A single commit in a repository.
 * Ask me questions about the specific commit.
 */
class RepoCommit {
  public shortHash: string;
  public longHash: string;
  public commitDate: Date;
  public timestamp: number;
  public repo: Repo;

  constructor({
    shortHash,
    longHash,
    commitDate,
    timestamp,
    repo,
  }: {
    shortHash: string;
    longHash: string;
    commitDate: Date;
    timestamp: number;
    repo: Repo;
  }) {
    this.shortHash = shortHash;
    this.longHash = longHash;
    this.commitDate = commitDate;
    this.timestamp = timestamp;
    this.repo = repo;
  }

  static create(args: {
    shortHash: string;
    longHash: string;
    commitDate: Date;
    timestamp: number;
    repo: Repo;
  }) {
    return new RepoCommit(args);
  }

  get date() {
    return this.commitDate;
  }

  /**
   * Get the link associated with this repo commit at a given path.
   *
   * TODO: not quite finished yet.
   */
  // historyLink(filePath: Path) {
  //   if (!this.repo.remoteUrl) {
  //     throw new Error(
  //       "Cannot create history link without a defined remote URL"
  //     );
  //   }

  //   return `${this.repo.remoteUrl}/blob/${
  //     this.longHash
  //   }/${filePath.toString()}`;
  // }
}

export default RepoCommit;
