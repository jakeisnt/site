import { Path } from "utils/path";
import logger from "utils/log";
import { sourceDir } from "../constants";
import type { PageSyntax } from "../types/html";

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

type Dependency = {
  src: string;
};

/**
 * Construct a dependency header with a list of source configurations.
 */
const makeDependencyHeader = (dependencies: Dependency[]): PageSyntax => {
  if (dependencies.length === 0) {
    return null;
  }

  return dependencies.map(({ src }) => {
    return getDependency(Path.create(src));
  });
};

const componentCache: { [key: string]: Function } = {};

/**
 * Require a component from disk.
 * @param name name of the component
 */
const requireComponent = (name: string) => {
  const maybeComponent = componentCache[name];
  if (maybeComponent) {
    return maybeComponent;
  }
  const rootPath = sourceDir;
  const componentFunction =
    require(`${rootPath}/components/${name}/${name}.js`).default;

  componentCache[name] = componentFunction;
  return componentFunction;
};

/**
 * Render a JS component.
 * @param name the name of the component
 * @param args arguments to pass to that component.
 * @returns the component
 */
const component = (name: string, args?: Object): PageSyntax => {
  const componentFunction = requireComponent(name);
  const { dependsOn, body } = componentFunction(args);
  const componentWithDependencies = [
    "span",
    body,
    makeDependencyHeader(dependsOn),
  ];

  logger.file("Rendering component", name);
  return componentWithDependencies;
};

export { component };
