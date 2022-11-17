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

var textValues = undefined;
var currentCharacter = undefined;
var characterSelector = document.getElementById("characterSelector");

/* Get the character names from the character selector. */
function getCharacterNames() {
  return Array.from(characterSelector.childNodes).map((option) => option.value);
}

// NOTE:
// Later, instead of swapping a->b,
// we want all scripts to have >2 characters
// and change all that are not the selected character to be "b".

function selectCharacter(e) {
  var selectedCharacter = event.target.value;
  console.log("selecting character" + selectedCharacter);

  if (!textValues) {
    textValues = getCharacterNames();
    currentCharacter = textValues[0];
  }

  if (selectedCharacter != currentCharacter) {
    switchGreentext();
    currentCharacter = selectedCharacter;
  }
}

characterSelector.addEventListener('change', selectCharacter);
console.log("selected character");
