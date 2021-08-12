function create(name, attrs, ... children) {
  return (parent) => {
    let elem = document.createElement(name);
    attrs && Object.keys(attrs).forEach((attr) => elem[attr] = attrs[attr]);
    (parent ?? document.body).appendChild(elem);
    children && children.forEach((childFn) => childFn(elem));
    return elem;
  }
}

function css(obj) {
  let classNames = null;
  const cssStr = (() => {
    switch(typeof obj) {
      case "string": return obj;
      default:
        classNames = getClassNames(obj);
        return objToCss(obj);
    }
  })();
  const styleSheet = document.createElement("style");
  styleSheet.type = "text/css";
  styleSheet.innerText = styles;
  document.head.appendChild(styleSheet);

  return classNames;
}

// convert a js object representing a stylesheet to a css string for the sheet
function objToCss(obj) {
  return Object.keys(obj).reduce((currCss, attr) => {
    let keyStyles = Object.keys(obj[attr]).reduce((currStyle, cssTag) =>
      `${currStyle ? currStyle + '\n' : ''}  ${dashCase(cssTag)}: ${obj[attr][cssTag]};`, false);
    return `${currCss ? currCss + '\n' : ''}${dashCase(attr)} {\n${keyStyles}\n}`;
  }, "");
}

function getClassNames(obj) {
  return Object.keys(obj).map(dashCase);
}

// convert a camelCase string to dash-case
function dashCase(str) {
  return str.replace(/([A-Z])/g, (match, group1) => `-${group1.toLowerCase()}`);
}

{
const exposedNames = { css, create };
  // bind to the ui namespace in the window
  if (typeof window === 'object') {
      window.UI = exposedNames;
  }
  // commonjs export
  if (typeof module === 'object' && module.exports) {
      module.exports = exposedNames;
  }
}
