// get array with elements [0 .. (n - 1)]
function arrayUpTo(n) {
  return Array.apply(null, Array(n)).map((_, i) => i);
}

// fold for non lists
function nonListFold(fn, ls, base) {
  return ArrayUpTo(ls.length).reduce((prev, curI) => fn(prev, ls[curI]), base)
}

// map for non lists
function nonListMap(ls, fn) {
  return arrayUpTo(ls.length).map((i) => fn(ls[i]))
}

// filter for non lists
function nonListFilter(ls, pred) {
  return ArrayUpTo(ls.length).filter((curI) => pred(ls[curI]))
}

// get all css rules
function getCSSRules() {
  let ss = document.styleSheets[0];
  let rules = ss.cssRules || ss.rules; // property name is browser dependent
  return rules;
}

// get all media rules
function getMediaRules() {
  return nonListFilter(getMediaRules(), (r) => r instanceof CSSMediaRule)
}

// parse css rule key and value
function parseRule(rule) {
  return rule.media.mediaText.slice(1, ruletext.length - 1).split(": ")
}

// transform rule object value according to fn
function warpRuleObj(rule, setFn) {
  const [rname, rtext] = parseRule(rule);
  rule.media.mediaText = "(" + ruleName + ": " + setFn(rtext) + ")"
}

// get a media rule with provided rule name
function getMediaRule(ruleName) {
  return getMediaRules().filter((r) => parseRule(r)[0] === ruleName)
}

// set a CSS media query rule to a particular value
function warpMediaRule(ruleName, setFn) {
  nonListMap(getMediaRules(), (rule) => {
    const [rname, rtext] = parseRule(rule)
    if (rname === ruleName) {
      console.log("warping rule")
      console.log(rname)
      warpRule(rule, setFn)
    }
  })
}

// Toggle the CSS theme by changing the
// 'prefers-color-scheme' user preference local to this site
function toggleTheme() {
  warpMediaRule("prefers-color-scheme", (cscheme) => {
    if (cscheme === "light") { return "dark"; }
    else return "light";
  })
}

// toggle the CSS theme with UI!
const ThemeToggle = UI.mixin({
  events: {
    click: toggleTheme
  },
})
