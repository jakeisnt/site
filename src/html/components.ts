import { Path } from "utils/path";
import { sourceDir } from "../constants";
import type { Dependency, PageSyntax } from "../types/html";
import type { PageSettings } from "../types/site";

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

/**
 * Convert a Path-specified dependency into an HTML tag.
 * Usually used to pull them into frontmatter.
 *
 * Places the dependencies where we expect them to be.
 * That is -- relative to targetDir.
 */
const getDependency = (path: Path, etc: object = {}): PageSyntax => {
  const extension = path.extension;
  switch (extension) {
    case "js":
      return ["script", { type: "module", ...etc, src: path.toString() }];
    case "ts":
      return getDependency(path.replaceExtension("js"), etc);
    case "css":
      return ["link", { rel: "stylesheet", ...etc, href: path.toString() }];
    case "scss":
      return getDependency(path.replaceExtension("css"), etc);
    default:
      throw new Error(`Unknown extension: ${extension}`);
  }
};

const getDependency2 = (
  { src, ...rest }: { src: Path },
  { sourceDir, targetDir }: PageSettings
) => {
  const resolvingPath = src.relativeTo(sourceDir, targetDir);
  return getDependency(resolvingPath, rest);
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

const parseDependencies = (deps: Dependency[]) => {
  return deps.map((dep) => ({ ...dep, src: Path.create(dep.src) }));
};

/**
 * Render a JS component.
 * @param name the name of the component
 * @param args arguments to pass to that component.
 * @returns the component
 */
const component = (
  name: string,
  args: Object | undefined,
  config: PageSettings
): { dependsOn: Dependency[]; body: PageSyntax } => {
  const componentFunction = requireComponent(name);
  const { dependsOn: dependsOnRaw, body } = componentFunction(args);

  const dependsOn = parseDependencies(dependsOnRaw);
  const componentWithDependencies = [
    dependsOn.map((v) => getDependency2(v, config)),
    body,
  ];

  return { dependsOn: dependsOnRaw, body: componentWithDependencies };
};

export { component };
