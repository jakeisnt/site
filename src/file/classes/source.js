import TextFile from './text';
import { htmlPage, header, component } from '../../html';

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'

const renderArticle = ({ articleHtml, file, siteName, url }) => {
  const title = file.name;

  return [
    "html",
    header({ title, url, siteName }),
    ["body",
     component('Sidebar', { path: file.path, title }),
     ["div", { class: 'site-body' },
      ["main",
       ["article", { class: 'wikipage' },
        ["h1", { class: 'title-top'}, title],
        articleHtml,
       ]
      ],
      ["div", { class: 'article-rhs-container' }
       ["div", { class: 'article-rhs'},
        component('GitHistoryTable', { file })]]]]];

        // component('PrevNextUpButtons', args)
}

// same args as above fn, but without the articleHtml
const renderSourceFile = ({ file }) => {
  const articleHtml = [
    "pre",
    ["code",
     { class: `language-${file.extension} has-raw-code` },
     file.text
    ]
  ];

  return renderArticle({ file, articleHtml });
}

class SourceFile extends TextFile {
  asHtml({ siteName, url }) {
    console.log(
      `rendering ${this.path} as html with siteName ${siteName} and url ${url}`
    );
    const page = renderSourceFile({ file: this, siteName, url });
    return htmlPage(page);
  }
}

export default SourceFile;
