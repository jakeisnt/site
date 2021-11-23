function arrayUpTo(n) {
  return Array.apply(null, Array(n)).map((_, i) => i);
}

function nonListMap(ls, fn) {
  return arrayUpTo(ls.length).map((i) => fn(ls[i]))
}

function getMediaRule() {

}

// set a CSS media query rule to a particular value
function setMediaRule(ruleName, ruleProp) {
  let ss = document.styleSheets[0];
  let rules = ss.cssRules || ss.rules; // property name is browser dependent

  nonListMap(rules, (rule) => {
    if (rule instanceof CSSMediaRule) {
      const [rname, rprop] = rule.media.mediaText.slice(1, ruletext.length - 1).split(": ")
      if (rname === ruleName) {
        rule.media.mediaText = "(" + ruleName + ": " + ruleProp + ")"
      }
    }
  })
}

setMediaRule("prefers-color-scheme", "light")


// console.log(document.styleSheets[0].cssRules)

// console.log(document.styleSheets[0].cssRules[0].media)


function toggleTheme() {
    // if it's light -> go dark
    if (themeStylesheet.href.includes('light')) {
      themeStylesheet.href = 'dark-theme.css';
      themeToggle.innerText = 'Switch to light mode';
    } else {
      // if it's dark -> go light
      themeStylesheet.href = 'light-theme.css';
      themeToggle.innerText = 'Switch to dark mode';
    }
}

// toggle the CSS theme with UI!
const ThemeToggle = UI.mixin({
  events: {
    click: toggleTheme
  },
})
