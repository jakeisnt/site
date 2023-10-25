const ScrollUp = () => ({
  dependsOn: [{ src: './scroll-up.js' }, { src: './scroll-up.css' }],
  body: ["div", { class: 'git-hist-table' },
         ["button", { class: 'scroll-up-button' }, "Scroll Up"]]
});

export default ScrollUp;
