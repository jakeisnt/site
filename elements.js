/* enhancements to native html elements */
const codeBlocks = document.getElementsByTagName('code');
console.log(codeBlocks);

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
    const codeContents = e.target.parentElement.innerText;
    const code = codeContents.substring(0, codeContents.length - e.target.innerText.length);
    navigator.clipboard.writeText(code);
  };
});

document.addEventListener('DOMContentLoaded', (event) => {
  document.querySelectorAll('pre code').forEach((block) => {
    hljs.highlightElement(block);
  });
});
