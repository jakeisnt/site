// a hiccup-like HTML domain specific language
// inspired by https://gist.github.com/hns/654226

import { isArray } from "utils/array";
import { isObject } from "utils/object";
import { PageSyntax, HtmlAttributes, HtmlTag } from "../types/html";

function html(...args: PageSyntax[]) {
  var buffer = [];
  build(args, buffer);
  return buffer.join("");
}

function htmlPage(...args: PageSyntax[]): string {
  return "<!DOCTYPE html>" + html(...args);
}

function build(list, buffer: string[]) {
  var index = 0;

  if (typeof list[index] === "string") {
    const [tagName, attr] = splitTag(list[index++]);

    if (isObject(list[index])) {
      mergeAttributes(attr, list[index++]);
    }
    buffer.push("<", tagName);

    for (var key in attr) {
      buffer.push(" ", key, '="', attr[key].toString(), '"');
    }
    buffer.push(">");
    buildRest(list, index, buffer);
    buffer.push("</", tagName, ">");
  } else {
    buildRest(list, index, buffer);
  }
}

function buildRest(list, index: number, buffer: string[]) {
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

function mergeAttributes(attr1: HtmlAttributes, attr2: HtmlAttributes) {
  for (var key in attr2) {
    if (!attr1.hasOwnProperty(key)) {
      attr1[key] = attr2[key];
    } else if (key === "class") {
      attr1[key] += " " + attr2[key];
    }
  }
}

function splitTag(tag: string): [HtmlTag, HtmlAttributes] {
  var attr: HtmlAttributes = {};
  var match = tag.match(/([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?/);
  if (match[2]) attr.id = match[2];
  if (match[3]) attr["class"] = match[3].replace(/\./g, " ");
  return [match[1] as HtmlTag, attr];
}

export { html, htmlPage };
