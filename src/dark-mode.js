// get all css rules
function getCSSRules() {
  let ss = document.styleSheets[0];
  let rules = ss.cssRules || ss.rules; // property name is browser dependent
  return rules;
}

// get all media rules
function getMediaRules() {
  return nonListFilter(getCSSRules(), (r) => r instanceof CSSMediaRule)
}

// parse css rule key and value
function parseRule(rule) {
  const ruletext = rule.media.mediaText;
  return ruletext.slice(1, ruletext.length - 1).split(": ")
}

// transform rule object value according to fn
function warpRule(rule, setFn) {
  const [rname, rtext] = parseRule(rule);
  rule.media.mediaText = "(" + rname + ": " + setFn(rtext) + ")"
}

// get a media rule with provided rule name
function getMediaRule(ruleName) {
  return parseRule(getMediaRules().filter((r) => parseRule(r)[0] === ruleName)[0])[1]
}

// set a CSS media query rule to a particular value
function warpMediaRule(ruleName, setFn) {
  nonListMap(getMediaRules(), (rule) => {
    const [rname, rtext] = parseRule(rule)
    if (rname === ruleName) {
      warpRule(rule, setFn)
    }
  })
}

// Toggle the CSS theme by changing the
// 'prefers-color-scheme' user preference local to this site
function toggleTheme() {
  return warpMediaRule("prefers-color-scheme",
                (cscheme) => cscheme === "light" ? "dark" : "light")
}

// is the browser currently in dark mode?
function isDarkMode() {
  return getMediaRule("prefers-color-scheme") === "dark";
}

// toggle the CSS theme with UI!
const ThemeToggle = UI.mixin({
  events: {
    click: toggleTheme
  },
})
