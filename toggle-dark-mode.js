/* A script to toggle dark mode with a button,
 * storing the current state in browser local storage.
 * The button hasn't yet been added...
 */
const THEME = {
  LIGHT: "light",
  DARK: "dark",
};

const possibleThemes = Object.values(THEME).map((theme) => `${theme}-theme`);
const btn = document.querySelector(".toggle-dark-mode");
const prefersDarkScheme = window.matchMedia("(prefers-color-scheme: dark)").matches;
let currentTheme = localStorage.getItem("theme") ?? (prefersDarkScheme ? THEME.DARK : THEME.LIGHT);

console.log("our current theme is ",currentTheme);

function switchToTheme(add) {
  const classes = document.body.classList;
  classes.remove(...possibleThemes);
  classes.add(`${add}-theme`);
  currentTheme = add;

  localStorage.setItem("theme", add);
}

function switchTheme() {
  if (currentTheme == THEME.DARK) {
    switchToTheme(THEME.LIGHT);
  } else {
    switchToTheme(THEME.DARK);
  }
}

btn.addEventListener("click", switchTheme);

switchToTheme(currentTheme);
