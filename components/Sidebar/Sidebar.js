const PATH_DELIMITER = ' / ';
import { component } from '../../src/html';

const collectFolderPaths = (pathList, title, curPath, { rootUrl, sourceDir }) => {
  if (curPath === undefined) {
    return collectFolderPaths(pathList, title, '', { rootUrl, sourceDir });
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
      "a", { href: `${rootUrl}${nextCurPath}/index.html` }, firstPath,
    ],
    ...collectFolderPaths(restPaths, title, nextCurPath, { rootUrl, sourceDir }),
  ];
}

const makeSidebar = ({ path, title, sourceDir, rootUrl }) => {
  const pathList = path.relativeTo(sourceDir).pathArray;
  console.log("pathList", pathList);
  const folderPaths = collectFolderPaths(pathList, title, null, { rootUrl, sourceDir });

  return [
    "div",
    { class: 'sidebar' },
    ["div",
     { class: 'url-path' },
     pathList?.length ? ["b", "jake."] : ["a", { href: '/' }, "jake."],
     folderPaths,
    ],
    component("ToggleDarkMode"),
  ]
}

const Sidebar = (args) => ({
   dependsOn: [{ src: '/components/Sidebar/sidebar.css' }],
   body: makeSidebar(args),
});

export default Sidebar;
