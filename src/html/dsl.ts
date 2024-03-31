// a hiccup-like HTML domain specific language
// inspired by https://gist.github.com/hns/654226

import { isArray } from "utils/array";
import { isObject } from "utils/object";

function html(...args) {
  var buffer = [];
  build(args, buffer);
  return buffer.join("");
}

function htmlPage(...args) {
  return "<!DOCTYPE html>" + html(...args);
}

function build(list, buffer) {
  var index = 0;

  if (typeof list[index] === "string") {
    var tag = splitTag(list[index++]);
    var attr = tag[1];
    tag = tag[0];
    if (isObject(list[index])) {
      mergeAttributes(attr, list[index++]);
    }
    buffer.push("<", tag);
    for (var key in attr) {
      buffer.push(" ", key, '="', attr[key], '"');
    }
    buffer.push(">");
    buildRest(list, index, buffer);
    buffer.push("</", tag, ">");
  } else {
    buildRest(list, index, buffer);
  }
}

function buildRest(list, index, buffer) {
  var length = list.length;
  while (index < length) {
    var item = list[index++];
    if (isArray(item)) {
      build(item, buffer);
    } else {
      buffer.push(item);
    }
  }
}

function mergeAttributes(attr1, attr2) {
  for (var key in attr2) {
    if (!attr1.hasOwnProperty(key)) {
      attr1[key] = attr2[key];
    } else if (key === "class") {
      attr1[key] += " " + attr2[key];
    }
  }
}

function splitTag(tag) {
  var attr = {};
  var match = tag.match(/([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?/);
  if (match[2]) attr.id = match[2];
  if (match[3]) attr["class"] = match[3].replace(/\./g, " ");
  return [match[1], attr];
}

export { html, htmlPage };
