function marquee() {
  function animateMarquee(element) {
    let elementWidth = element.offsetWidth;
    let parentWidth = element.parentElement.offsetWidth;
    let flag = 0;

    setInterval(() => {
        element.style.marginLeft = --flag + "px";

        if (elementWidth == -flag) {
            flag = parentWidth;
        }
    }, 10);
  }

  UI.css(`
    #marquee_container {
      border: 1px solid;
      background: yellow;
      width: 100%;
      overflow: hidden;
      position: fixed;
      top: 0;
      left: 0;
    }
    #marquee_body {
      color: black;
      font-weight: bold;
      white-space: nowrap;
      clear: both;
      float: left;
    }
    `)

  const container = UI.create(
    "div",
    { id: "marquee_container" },
    UI.create("div", { id: "marquee_body" }, "WELCOME")
  )();

  animateMarquee(container.children[0].node);
}

marquee();
