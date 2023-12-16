import { Path } from "utils/path";
import logger from "utils/log";
import { sourceDir } from "../constants";

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

const getDependency = (path) => {
  const extension = path.extension;
  switch (extension) {
    case "js":
      return ["script", { src: path.pathString }];
    case "css":
      return ["link", { rel: "stylesheet", href: path.pathString }];
    case "scss":
      return getDependency(path.replaceExtension("css"));
    default:
      throw new Error(`Unknown extension: ${extension}`);
  }
};

const makeDependencyHeader = (dependencies) => {
  if (dependencies.length === 0) {
    return null;
  }

  return dependencies.map(({ src }) => {
    return getDependency(Path.create(src));
  });
};

const component = (name, args) => {
  const rootPath = sourceDir;
  const componentFunction = require(`${rootPath}/components/${name}/${name}.js`);
  logger.file("Rendering component", name);
  const { dependsOn, body } = componentFunction.default(args);
  return ["span", body, makeDependencyHeader(dependsOn)];
};

export { component };
