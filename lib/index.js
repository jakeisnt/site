const {
  isMobile
} = require('./isMobile');

const {
  css
} = require('./css');

const {
  create
} = require('./ui');

const exposedNames = { isMobile, css, create };

// If there is a global `window` object, bind API names to it.
if (typeof window === 'object') {
    window.UI = exposedNames;
}
// Export public APIs CommonJS-style
if (typeof module === 'object' && module.exports) {
    module.exports = exposedNames;
}
