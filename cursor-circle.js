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

  // Your CSS as text
  // https://stackoverflow.com/questions/707565/how-do-you-add-css-with-javascript
  let styles = `
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

  var styleSheet = document.createElement("style")
  styleSheet.type = "text/css"
  styleSheet.innerText = styles
  document.head.appendChild(styleSheet)

  let cursor = document.createElement("div");
  cursor.id = "invertedcursor";
  document.body.appendChild(cursor);
}

circularCursor();
