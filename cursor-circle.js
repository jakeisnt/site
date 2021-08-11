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


  const css = {
    "#test-tag": {
      borderRadius: 3,
      position: "absolute",
    }
  }

  // convert a js object representing a stylesheet to a css string for the sheet
  function objToCss(obj) {
    return Object.keys(obj).reduce((currCss, attr) => {
      let keyStyles = Object.keys(obj[attr]).reduce((currStyle, cssTag) =>
        `${currStyle ? currStyle + '\n' : ''}  ${dashCase(cssTag)}: ${obj[attr][cssTag]};`, false);
      return `${currCss ? currCss + '\n' : ''}${dashCase(attr)} {\n${keyStyles}\n}`;
    }, "");
  }

  console.log(objToCss(css));

  // determine whether a string is dash-cased
  function isDashCase(str) {
    // vestigial regex:
    // [[a-z]|-]*\w+
    // return !(/^[A-Z]/.test(word));
    // a dash case string simply does not contain capital letters
   return word === word.toLowerCase();
  }

  // convert a dash-delimited string to camelCase
  function camelCase(str) {
    return input.toLowerCase().replace(/-(.)/g, function(match, group1) {
        return group1.toUpperCase();
    });
  }

  // convert a camelCase string to dash-case
  function dashCase(str) {
    return str.toLowerCase().replace(/([A-Z])/g, (match, group1) => `-${group1.toLowerCase()}`);
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
