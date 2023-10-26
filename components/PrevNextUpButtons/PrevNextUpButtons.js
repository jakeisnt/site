const prevNextUpHtml = ({ file, rootUrl, sourceDir }) => {
  const prevFile = file.prevFile;
  const nextFile = file.nextFile;
  const dir = file.directory;

  return [
    "div",
    { class: 'prev-next-up-buttons-container' },
    "Navigation",
    ["table", { class: 'prev-next-up-buttons' },
     (prevFile && [
       "tr",
       ["td", "Previous"],
       ["td", ["a", { class: 'prev-button', href: prevFile.htmlUrl({ rootUrl, sourceDir }) }, prevFile.name]]
     ]),
     (nextFile && [
       "tr",
       ["td", "Previous"],
       ["td", ["a", { class: 'next-button', href: nextFile.htmlUrl({ rootUrl, sourceDir }) }, prevFile.name]]
     ]),
     (dir && [
       "tr",
       ["td", "Up"],
       ["td", ["a", { class: 'up-button', href: dir.htmlUrl({ rootUrl, sourceDir }) }, dir.name]]
     ]),
    ]
  ];
}

const PrevNextUpButtons = (args) => ({
  dependsOn: [],
  body: prevNextUpHtml(args),
});

export default PrevNextUpButtons;
