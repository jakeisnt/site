/* A script to toggle dark mode with a button,
 * storing the current state in browser local storage.
 * The button hasn't yet been added...
 */

(() => {
  function svg(url) {
    let obj = document.createElement("div");
    obj.mask = "url(" + url + ")";
    return obj;
  }

  const THEME = {
    LIGHT: "light",
    DARK: "dark",
  };

  const ICONS = {
    light: svg("/components/toggle-dark-mode/assets/sun_icon.svg"),
    dark: svg("/components/toggle-dark-mode/assets/moon_icon.svg"),
  }

  const possibleThemes = Object.values(THEME).map((theme) => `${theme}-theme`);
  const btn = document.querySelector(".toggle-dark-mode");
  const prefersDarkScheme = window.matchMedia("(prefers-color-scheme: dark)").matches;
  let currentTheme = localStorage.getItem("theme") ?? (prefersDarkScheme ? THEME.DARK : THEME.LIGHT);


  function switchToTheme(add) {
    console.log("Switching to theme ", add)
    const classes = document.body.classList;
    classes.remove(...possibleThemes);
    classes.add(`${add}-theme`);
    currentTheme = add;

    localStorage.setItem("theme", add);
  }

  function switchTheme() {
    if (currentTheme == THEME.DARK) {
      switchToTheme(THEME.LIGHT);
      btn.replaceChild(ICONS.light, ICONS.dark);
    } else {
      switchToTheme(THEME.DARK);
      btn.replaceChild(ICONS.dark, ICONS.light);
    }
  }

  btn.addEventListener("click", switchTheme);
  switchToTheme(currentTheme);
  btn.appendChild(ICONS[currentTheme]);
})();
