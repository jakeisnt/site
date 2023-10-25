import { html } from './dsl';
import { Path } from '../utils/path';

// Support the Component interface.
// Resolves dependencies automatically and allows partial page refresh.

const getDependency = (filename) => {
  const extension = getExtension(filename);
  switch (extension) {
    case 'js':
      return ['script', { src: filename }];
    case 'css':
      return ['link', { rel: 'stylesheet', href: filename }];
    case 'scss':
      return getDependency(Path.create(filename).replaceExtension('css'));
    default:
      throw new Error(`Unknown extension: ${extension}`);
  }
};

const makeDependencyHeader = (dependencies) => {
   return dependencies.map((dependency) => {
     const filename = dependency.filename;
     return getDependency(filename);
   }).join('\n');
 }

const component = (name, file, files, fileListIndex, compiledHtml) => {
  const componentFunction = require(`/home/jake/site/components/${name}/${name}.js`).default;
  const {
    dependsOn,
    body,
  } = componentFunction(file, files, fileListIndex, compiledHtml);
  return html("span", body, makeDependencyHeader(dependsOn));
}

export { component };
