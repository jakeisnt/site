function print(...txt: any[]) {
  return console.log(txt);
}

function runOnDesktop(fn: Function) {
  if (window.matchMedia("(min-width: 501px)")) {
    return fn();
  }
}

type ElementNames = "img" | "a" | "h1";

/**
 * Create an html element with attributes and append it to a parent element
 * @param elementName
 * @param attributes
 * @param parent
 * @returns
 */
function create(
  elementName: ElementNames,
  attributes: { [key: string]: any },
  parent: HTMLElement
) {
  const elem = document.createElement(elementName);
  for (let key in attributes) {
    // some things only work one way, so we do both
    // is this faster than a switch statement? not sure.
    elem.setAttribute(key, attributes[key]);
    elem[key] = attributes[key];
  }

  if (parent) {
    parent.appendChild(elem);
  }

  return elem;
}

function create2(elementName, attributes, ...children) {
  const elem = document.createElement(elementName);
  for (let key in attributes) {
    // some things only work one way, so we do both
    // is this faster than a switch statement? not sure.
    elem.setAttribute(key, attributes[key]);
    elem[key] = attributes[key];
  }

  if (children) {
    children.forEach((child) => {
      if (typeof child === "string") {
        elem.appendChild(document.createTextNode(child));
      } else {
        elem.appendChild(child);
      }
    });
  }

  return elem;
}

var httpRequest: XMLHttpRequest;

function req(url, method, then) {
  if (window.XMLHttpRequest) {
    httpRequest = new XMLHttpRequest();
  } else if (window.ActiveXObject) {
    httpRequest = new ActiveXObject("Microsoft.XMLHTTP");
  }

  httpRequest.onreadystatechange = function () {
    if (httpRequest.readyState === XMLHttpRequest.DONE) {
      if (httpRequest.status === 200) {
        // All set
        var response = JSON.parse(httpRequest.responseText);
        then(response);
      } else {
        console.log("There was a problem with the last.fm request.");
      }
    }
  };

  httpRequest.open(method, url, true);
  httpRequest.send();
}

const get = (url, then) => req(url, "GET", then);

const $ = (selector) => document.querySelector(selector);
const all = (selector) => Array.from(document.querySelectorAll(selector));

function dynLoad(src, id) {
  var s = document.createElement("script");
  s.setAttribute("src", src);
  s.setAttribute("id", id);
  s.setAttribute("async", "true");
  return s;
}
