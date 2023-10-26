import TextFile from './text';
import { htmlPage, header, component } from '../../html';

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'

const renderArticle = ({ articleHtml, file, siteName, rootUrl, sourceDir }) => {
  const title = file.name;

  return [
    "html",
    header({ title, rootUrl, siteName }),
    ["body",
     component('Sidebar', { path: file.path, title }),
     ["div", { class: 'site-body' },
      ["main",
       ["article", { class: 'wikipage' },
        ["h1", { class: 'title-top'}, title],
        articleHtml,
       ]
      ],
      ["div", { class: 'article-rhs-container' },
       ["div", { class: 'article-rhs'},
        component('GitHistoryTable', { file }),
        component('PrevNextUpButtons', { file, rootUrl, sourceDir })]]]]];
}

// same args as above fn, but without the articleHtml
const renderSourceFile = ({ file, rootUrl, siteName, sourceDir }) => {
  const articleHtml = [
    "pre",
    ["code",
     { class: `language-${file.extension} has-raw-code` },
     file.text
    ]
  ];

  return renderArticle({ file, articleHtml, rootUrl, siteName, sourceDir });
}

class SourceFile extends TextFile {
  asHtml({ siteName, rootUrl, sourceDir }) {
    console.log(
      `rendering ${this.path} as html with siteName ${siteName} and url ${rootUrl}`
    );
    const page = renderSourceFile({ file: this, siteName, rootUrl, sourceDir });
    return htmlPage(page);
  }
}

export default SourceFile;
