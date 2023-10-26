const makeGitHistoryTable = ({ file }) => {
  const log = file.log;

  return [
    "div", { class: 'git-history-table-container'},
    ["span", { class: 'git-history-table-title' }, "Revisions"],
    ["table", { class: 'git-history-table' },
    ["tr",
     ["th", "Date"],
     ["th", "Hash"]]
     ["tbody",
      log.map((entry) => {
        return ["tr",
                ["td", { class: 'commit-date-tr' }, entry.date],
                ["td", { class: 'commit-link-tr' }, entry.shortHash]];
      })]]];
};


const GitHistoryTable = ({ file }) => ({
  dependsOn: [],
  body: makeGitHistoryTable({ file }),
});

export default GitHistoryTable;
