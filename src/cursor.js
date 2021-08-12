// final goal:
// mimic how this one works! https://mtg-interieur.fr/
// - small circle leading large cursor
// - maybe inversion of text under? maybe covering the text?
// - 'absorbed' into larger bubbles that react when about to click on them!

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

  UI.css(styles);
  UI.create("div", {id: "invertedcursor"});
}

circularCursor();
