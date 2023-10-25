import { html } from './dsl';
import { Path } from '../utils/path';

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

const getDependency = (path) => {
  const extension = path.extension;
  switch (extension) {
    case 'js':
      return ['script', { src: path.pathString }];
    case 'css':
      return ['link', { rel: 'stylesheet', href: path.pathString }];
    case 'scss':
      return getDependency(path.replaceExtension('css'));
    default:
      throw new Error(`Unknown extension: ${extension}`);
  }
};

const makeDependencyHeader = (dependencies) => {
   return dependencies.map(({ src }) => {
     return getDependency(Path.create(src));
   }).join('\n');
 }

const component = (name, args) => {
  const componentFunction = require(`/home/jake/site/components/${name}/${name}.js`);

  console.log('componentFunction', componentFunction);
  const {
    dependsOn,
    body,
  } = componentFunction.default(args);
  return html("span", body, makeDependencyHeader(dependsOn));
}

export { component };
