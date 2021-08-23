function addContents(elem, text) {
  elem.innerText += text;
  return { text: text };
}

// assign attributes to an html element
function assignAttributes(elem, attrs) {
    // if events are declared, add all of them
    if(attrs.events) {
        Object.keys(attrs.events).forEach((eventName) => {
          // needed to keep the event in the closure, as we delete the ref later
          const event = attrs.events[eventName];
          elem.addEventListener(eventName, (e) => event(e, elem));
        });
    }

    // delete the key so the attribute isn't indexed onto the html!
    // is there a better solution to this?
    delete attrs.events;

    // index all keys as html attribs
    attrs && Object.keys(attrs).forEach((attr) => {
      elem[attr] = attrs[attr];
    });
}

function create(name, attrs, ... children) {
  return (parent) => {
    let elem = document.createElement(name);

    attrs && assignAttributes(elem, attrs);

    // add the element to the DOM
    (parent ?? document.body).appendChild(elem);

    // render all of the child components!!
    // maybe: define 'children' for the parent node and vice versa -
    // which would give us a real graph up and down the component tree!
    const childNodes = children && children.map(childFn => {
      return (typeof childFn === "string" ? addContents(elem, childFn) : childFn(elem))
      });

    // give us back the whole element tree with attrs!
    return {...attrs, node: elem, children: childNodes };
  }
}

// cheese an svg onto the document as a string
function svgString(parent, str) {
  const node = document.createRange().createContextualFragment(str);
  parent.appendChild(node);
}

// TODO: looks awfully similar to 'create'...
function svg(name, attrs, ...children) {
  return parent => {
    const node = document.createElementNS("http://www.w3.org/2000/svg", name);
    attrs && Object.keys(attrs).forEach(attrName => node.setAttribute(attrName, attrs[attrName]));
    const childNodes = children && children.map(childFn => (typeof childFn === "string" ? svgString(node, childFn) : childFn(node)));

    // add the element to the DOM
    (parent ?? document.body).appendChild(node);

    return {...attrs, node: node, children: childNodes };
  };
}

// creates a mixin that adds attributes to a node
function mixin(attrs) {
  return function(childFn) {
    return function() {
      const child = childFn()
      assignAttributes(child.node, attrs);
      return { ...child, attrs };
    }
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
const exposedNames = { svg, svgString, css, create, mixin };
  // bind to the ui namespace in the window
  if (typeof window === 'object') {
      window.UI = exposedNames;
  }
  // commonjs export
  if (typeof module === 'object' && module.exports) {
      module.exports = exposedNames;
  }
}
