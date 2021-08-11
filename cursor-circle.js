import { css } from './css.js';

function create(name, attrs, parent) {
  let elem = document.createElement(name);
  elem = { ... elem, ...attrs };
  (parent ?? document.body).appendChild(elem);
  return elem;
}

// Your CSS as text
// https://stackoverflow.com/questions/707565/how-do-you-add-css-with-javascript
const styles = `
    #invertedcursor {
      position: absolute;
      width: 40px;
      height: 40px;
      background: #fff;
      border-radius: 50%;
      top: var(--y, 0);
      left: var(--x, 0);
      transform: translate(-50%, -50%);
      z-index: 2;
      mix-blend-mode: difference;
      transition: transform .2s;
    }
`

function circularCursor() {
  document.body.onmousemove = function(e) {
    document.documentElement.style.setProperty (
      '--x', (
        e.clientX+window.scrollX
      )
        + 'px'
    );
    document.documentElement.style.setProperty (
      '--y', (
        e.clientY+window.scrollY
      )
        + 'px'
    );
  }

  css(styles);
  create("div", {id: "invertedcursor"});
}

circularCursor();
