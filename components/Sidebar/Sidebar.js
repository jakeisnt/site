const PATH_DELIMITER = ' / ';

const collectFolderPaths = (pathList, title, curPath) => {
  if (curPath === undefined) {
    return collectFolderPaths(pathList, title, '');
  }

  if (!pathList?.[1]) {
    return [
      ["span", PATH_DELIMITER],
      ["b", title],
    ];
  }

  const firstPath = pathList[0];
  const restPaths = pathList.slice(1);

  const nextCurPath = curPath + '/' + firstPath;

  return [
    ["span", PATH_DELIMITER],
    [
      "a", { href: `/${curPath}/index.html` }, firstPath,
    ],
    ...collectFolderPaths(restPaths, title, nextCurPath),
  ];
}

const makeSidebar = (path, title) => {
  const pathList = path.pathArray;
  const folderPaths = collectFolderPaths(pathList, title);

  return [
    "div",
    { class: 'sidebar' },
    ["div",
     { class: 'url-path' },
     pathList?.length ? ["b", "jake."] : ["a", { href: '/' }, "jake."],
     folderPaths,
    ]
    // component("ToggleDarkMode"),
  ]
}

const Sidebar = ({ path, title }) => ({
   dependsOn: [{ src: '/components/Sidebar/sidebar.css' }],
   body: makeSidebar(path, title),
});

export default Sidebar;
