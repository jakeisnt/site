const makeGitHistoryTable = ({ file }) => {
  const log = file.log;

  const tableRows = log.map((entry) => {
    return ["tr",
            ["td", { class: 'commit-date-tr' }, entry.date],
            ["td", { class: 'commit-link-tr' }, entry.shortHash]];
  });

  const table = [
    "div", { class: 'git-history-table-container'},
    ["span", { class: 'git-history-table-title' }, "Revisions"],
    ["table", { class: 'git-history-table' },
     ["thead",
      ["tr",
       ["th", "Date"],
       ["th", "Hash"]]],
      ["tbody", ...tableRows]]];

  return table;
};


const GitHistoryTable = ({ file }) => ({
  dependsOn: [],
  body: makeGitHistoryTable({ file }),
});

export default GitHistoryTable;
