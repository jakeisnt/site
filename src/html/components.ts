import { Path } from "utils/path";
import logger from "utils/log";
import { sourceDir } from "../constants";

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

const getDependency = (path: Path) => {
  const extension = path.extension;
  switch (extension) {
    case "js":
      return ["script", { src: path.toString() }];
    case "css":
      return ["link", { rel: "stylesheet", href: path.toString() }];
    case "scss":
      return getDependency(path.replaceExtension("css"));
    default:
      throw new Error(`Unknown extension: ${extension}`);
  }
};

const makeDependencyHeader = (dependencies: { src: string }[]) => {
  if (dependencies.length === 0) {
    return null;
  }

  return dependencies.map(({ src }) => {
    return getDependency(Path.create(src));
  });
};

/**
 * Render a JS component.
 * @param name the name of the component
 * @param args arguments to pass to that component.
 * @returns the component
 */
const component = (name: string, args?: Object) => {
  const rootPath = sourceDir;
  const componentFunction = require(`${rootPath}/components/${name}/${name}.js`);
  logger.file("Rendering component", name);
  const { dependsOn, body } = componentFunction.default(args);
  return ["span", body, makeDependencyHeader(dependsOn)];
};

export { component };
