import { Path } from "utils/path";
import logger from "utils/log";
import { sourceDir } from "../constants";
import { PageSyntax } from "../types/html";

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

/**
 * Convert a Path-specified dependency into an HTML tag.
 * Usually used to pull them into frontmatter.
 */
const getDependency = (path: Path): PageSyntax => {
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

/**
 * Construct a dependency header with a list of source configurations.
 */
const makeDependencyHeader = (dependencies: { src: string }[]): PageSyntax => {
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
const component = (name: string, args?: Object): PageSyntax => {
  const rootPath = sourceDir;
  const componentFunction = require(`${rootPath}/components/${name}/${name}.js`);
  logger.file("Rendering component", name);
  const { dependsOn, body } = componentFunction.default(args);
  return ["span", body, makeDependencyHeader(dependsOn)];
};

export { component };
