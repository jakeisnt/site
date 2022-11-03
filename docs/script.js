function changeClassName(old, new) {
  Array.from(document.getElementsByClassName(old)).forEach((elem) => { elem.className = new; });
}

function switchGreentext() {
  changeClassName("a", "temp");
  changeClassName("b", "a");
  changeClassName("temp", "b");
}
