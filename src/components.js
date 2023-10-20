import { html } from './html.js';

const getExtension = (filename) => {
  const i = filename.lastIndexOf('.');
  return (i < 0) ? '' : filename.substr(i);
};

const swapExtension = (filename, newExtension) => {
  const i = filename.lastIndexOf('.');
  return (i < 0) ? filename + newExtension : filename.substr(0, i) + newExtension;
};

const getDependency = (filename) => {
  const extension = getExtension(filename);
  switch (extension) {
    case 'js':
      return html`<script src="${filename}"></script>`;
    case 'css':
      return html`<link rel="stylesheet" href="${filename}">`;
    case 'scss':
      return getDependency(swapExtension(filename, 'css'));
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
  const componentFunction = readFileSync(`/home/jake/site/components/${name}/${name}.js`, 'utf8');
  const {
    dependsOn,
    body,
  } = componentFunction(file, files, fileListIndex, compiledHtml);

  return html("span", body, makeDependencyHeader(dependsOn));
}
