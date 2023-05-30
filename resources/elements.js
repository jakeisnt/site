/* enhancements to native html elements */

/* code blocks */
const codeBlocks = document.querySelectorAll('pre code');

Array.from(codeBlocks).forEach((codeBlock) => {
  const btn = document.createElement('button');
  btn.classList.add('inset');
  btn.innerText = 'Copy';
  codeBlock.parentElement.style = 'position: relative';
  codeBlock.parentElement.appendChild(btn);

  const label = document.createElement('div');
  label.classList.add('label');
  label.classList.add('inset')
  label.innerText = codeBlock.classList[0].replace('language-', '');
  codeBlock.parentElement.appendChild(label);

  btn.onclick = function(e) {
    const codeContents = codeBlock.innerText;
    navigator.clipboard.writeText(codeContents);
  };
});

document.addEventListener('DOMContentLoaded', (event) => {
  document.querySelectorAll('pre code').forEach((block) => {
    hljs.highlightElement(block);
  });
});

/* Allow users to copy links from headings */
const headings = document.querySelectorAll('h1, h2, h3, h4, h5, h6');

Array.from(headings).forEach((heading) => {
  const headingId = heading.id;
  const btn = document.createElement('button');
  btn.classList.add('inlineText');
  btn.innerText = '#';
  heading.prepend(btn);

  btn.onclick = function(e) {
    console.log(e.target);
    const url = window.location.origin + window.location.pathname;
    const link = `${url}#${headingId}`;
    navigator.clipboard.writeText(link);
  };
});

/* Support footnotes */
const footnoteRefs = document.querySelectorAll('sup');

footnoteRefs.forEach((footnoteRef) => {
  footnoteRef.classList.add('footnoteRef');
  const footnoteId = footnoteRef.id;
  const refNum = footnoteId.replace('fnref-', '');
  footnoteRef.children[0].innerText = `[${refNum}]`;
});

const footnotes = document.querySelectorAll('li[id^="fn-"]');
footnotes.forEach((footnote) => {
  footnote.classList.add('footnote');
});
