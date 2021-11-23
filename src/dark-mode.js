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
