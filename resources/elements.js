/* enhancements to native html elements */

/* code blocks */
const codeBlocks = document.querySelectorAll('pre code');

Array.from(codeBlocks).forEach((codeBlock) => {
  const btn = create('button', {
    className: 'inset',
    innerText: 'Copy',
    onclick: (e) => {
      const codeContents = codeBlock.innerText;
      navigator.clipboard.writeText(codeContents);
    },
  });

  codeBlock.parentElement.style = 'position: relative';
  codeBlock.parentElement.appendChild(btn);

  const label = create('div', {
    className: 'label inset',
    innerText: codeBlock.classList[0].replace('language-', ''),
  });

  codeBlock.parentElement.appendChild(label);
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
  const btn = create('button', {
    className: 'inlineText',
    innerText: '#',
    onclick: (e) => {
        console.log(e.target);
        const url = window.location.origin + window.location.pathname;
        const link = `${url}#${headingId}`;
        navigator.clipboard.writeText(link);
      },
  });

  heading.prepend(btn);
});

// highlight current link in nav
document.addEventListener('scroll', (event) => {
  const scrollPosition = window.scrollY;

  for(let i = 0; i < headings.length; i++) {
    const headingId = headings[i].id;
    const nextHeadingId = headings[i + 1] ? headings[i + 1].id : null;
  }
  Array.from(headings).forEach((heading) => {
    const headingId = heading.id;
    const section = document.getElementById(sectionId);
    const sectionTop = section.offsetTop;
    const sectionBottom = sectionTop + section.offsetHeight;

    if (scrollPosition >= sectionTop && scrollPosition < sectionBottom) {
      link.classList.add('active');
    } else {
      link.classList.remove('active');
    }
  });
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

