import { TextFile } from '../file';
import { htmlPage, header } from '../../html';

// if it's a source code file, we want to:
// - render both to 'file.$ext' and 'file.$ext.html'
// - hide 'file.$ext' from the index
// - show 'file.$ext.html' in the index, looking like 'file.$ext'

const renderArticle = (args) => {
  const { articleHtml, file, files, fileListIdx } = args;
  const pageName = file.name;

  return [
    "html",
    header(),
    ["body",
     component('Sidebar', args),
     ["div", { class: 'site-body' },
      ["main",
       ["article", { class: 'wikipage' },
        ["h1", { class: 'title-top'}, pageName],
        articleHtml,
       ]
      ],
      ["div", { class: 'article-rhs-container' }
       ["div", { class: 'article-rhs'}
        component('GitHistoryTable', args),
        component('PrevNextUpButtons', args)]]]]];
}

// same args as above fn, but without the articleHtml
const renderSourceFile = (args) => {
  const { file } = args;

  const articleHtml = [
    "pre",
    ["code",
     { class: `language-${file.extension} has-raw-code` },
     file.toString()
    ]
  ];

  return renderArticle({ ...args, articleHtml });
}

class SourceFile extends TextFile {}

export default SourceFile;
