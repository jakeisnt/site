const PATH_DELIMITER = ' / ';
import { component } from '../../src/html';

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

  console.log("firstPath", firstPath, 'restPath', restPaths);
  const nextCurPath = curPath + '/' + firstPath;
  console.log("nextCurPath", nextCurPath);

  return [
    ["span", PATH_DELIMITER],
    [
      "a", { href: `http://localhost:4242${nextCurPath}/index.html` }, firstPath,
    ],
    ...collectFolderPaths(restPaths, title, nextCurPath),
  ];
}

const makeSidebar = (path, title) => {
  const pathList = path.relativeTo('/home/jake/site').pathArray;
  console.log("pathList", pathList);
  const folderPaths = collectFolderPaths(pathList, title);

  console.log(folderPaths);

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

const Sidebar = ({ path, title }) => ({
   dependsOn: [{ src: '/components/Sidebar/sidebar.css' }],
   body: makeSidebar(path, title),
});

export default Sidebar;
