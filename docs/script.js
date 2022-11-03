function changeClassName(old, neew) {
  // note: this replacement assumes that the replaced classname is not hte first classname to be modified
  Array.from(document.getElementsByClassName(old)).forEach(
    (elem) => { console.log(elem.className); elem.className = elem.className.replace(` ${old}`, ` ${neew}`); });
}

function swapClassNames(a, b) {
  changeClassName(a, "temp");
  changeClassName(b, a);
  changeClassName("temp", b);
}

function switchGreentext() {
  swapClassNames("a", "b");
  swapClassNames("current", "other");
}
