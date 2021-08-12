function create(name, attrs, ... children) {
  return (parent) => {
    let elem = document.createElement(name);

    // if events are declared, add all of them
    if(attrs.events) {
        Object.keys(attrs.events).forEach((eventName) => {
          elem.addEventListener(eventName, attrs.events[eventName]);
        });
    }

    // delete the key so the attribute isn't indexed onto the html!
    // hopefully not needed later... if it is, we can vdom it (probably?)
    delete attrs.events;

    // index all keys as html attribs
    attrs && Object.keys(attrs).forEach((attr) => {
      elem[attr] = attrs[attr];
    });

    // add the element to the DOM
    (parent ?? document.body).appendChild(elem);

    // render all of the child components!!
    // maybe: define 'children' for the parent node and vice versa -
    // which would give us a real graph up and down the component tree!
    children && children.forEach((childFn) => childFn(elem));

    // can get this element back if we want : )
    return elem;
  }
}

let styleSheet = null;

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

  if(!styleSheet) {
    styleSheet = document.createElement("style");
    styleSheet.type = "text/css";
    styleSheet.innerText = cssStr;
    document.head.appendChild(styleSheet);
  } else {
    styleSheet.innerText += cssStr;
  }

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
