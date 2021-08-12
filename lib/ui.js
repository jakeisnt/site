function create(name, attrs, parent) {
  let elem = document.createElement(name);
  Object.keys(attrs).forEach((attr) => elem[attr] = attrs[attr]);
  document.body.appendChild(elem);
  return elem;
}

function css(obj) {
  const cssStr = (() => {
    switch(typeof obj) {
    case "string": return obj;
    default: return objToCss(obj);
    }
  })();

  const styleSheet = document.createElement("style");
  styleSheet.type = "text/css";
  styleSheet.innerText = styles;
  document.head.appendChild(styleSheet);
}

// convert a js object representing a stylesheet to a css string for the sheet
function objToCss(obj) {
  return Object.keys(obj).reduce((currCss, attr) => {
    let keyStyles = Object.keys(obj[attr]).reduce((currStyle, cssTag) =>
      `${currStyle ? currStyle + '\n' : ''}  ${dashCase(cssTag)}: ${obj[attr][cssTag]};`, false);
    return `${currCss ? currCss + '\n' : ''}${dashCase(attr)} {\n${keyStyles}\n}`;
  }, "");
}

// convert a camelCase string to dash-case
function dashCase(str) {
  return str.replace(/([A-Z])/g, (match, group1) => `-${group1.toLowerCase()}`);
}

const exposedNames = { css, create };

// bind to the ui namespace in the window
if (typeof window === 'object') {
    window.UI = exposedNames;
}
// commonjs export
if (typeof module === 'object' && module.exports) {
    module.exports = exposedNames;
}
