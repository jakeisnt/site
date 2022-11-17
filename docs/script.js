function changeClassName(old, neew) {
  // note: this replacement assumes that the replaced classname is not hte first classname to be modified
  Array.from(document.getElementsByClassName(old)).forEach(
    (elem) => {
      console.log(elem.className);
      elem.className = elem.className.replace(` ${old}`, ` ${neew}`);
      console.log(elem.className);
    });
}

function swapClassNames(a, b) {
  changeClassName(a, "temp");
  changeClassName(b, a);
  changeClassName("temp", b);
}

var characters = undefined;
var currentCharacter = undefined;
var characterSelector = document.getElementById("characterSelector");

/* Get the character names from the character selector. */
function getCharacterNames() {
  return Array.from(characterSelector.childNodes).map((option) => option.value);
}

/* Fetch info about the current characters from the `<select> element.` */
function cacheCharacters() {
  characters = getCharacterNames();
  currentCharacter = characters[0];
}

/* Make the chosen character the current character. */
function makeCharacterCurrent(characterName) {
  if (characterName != currentCharacter) {
    console.log(`switching to character ${characterName}`)
    changeClassName("right", "left"); // move all to the left
    changeClassName(`${characterName} left`, `${characterName} right`); // make current character the current 'a'
    currentCharacter = characterName;
  }
}

function selectCharacter(e) {
  var selectedCharacter = event.target.value;
  if (!characters) { cacheCharacters(); }
  makeCharacterCurrent(selectedCharacter);
}

console.log(characterSelector);
characterSelector.addEventListener('change', selectCharacter);
