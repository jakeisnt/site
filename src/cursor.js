// final goal:
// mimic how this one works! https://mtg-interieur.fr/
// - small circle leading large cursor
// - maybe inversion of text under? maybe covering the text?
// - 'absorbed' into larger bubbles that react when about to click on them!

const styles = `
    #invertedcursor {
      position: absolute;
      width: 40px;
      height: 40px;
      background: #fff;
      border-radius: 50%;
      top: 0;
      left: 0;
      transform: translate(-50%, -50%);
      z-index: 2;
      mix-blend-mode: difference;
      transition: transform .2s;
      pointer-events: none;
    }
    #funcircle {
      position: absolute;
      width: 750px;
      height: 2200px;
      background: #fff;
      border-radius: 50%;
      top: 0;
      left: 0;
      transform: translate(-50%, -50%);
      z-index: 2;
      mix-blend-mode: difference;
      transition: transform .2s;
      pointer-events: none;
    }
`

function circularCursor() {
  UI.css(styles);
  const cursor = UI.create("div", {id: "invertedcursor"});
  UI.create("div", {id: "funcircle"});

  // document and window can both listen for this event;
  // document is higher in the listener hierarchy so it triggers first,
  // though either can perform either functionality
  window.addEventListener("mousemove", (e) => {
    cursor.style.top = e.pageY + "px";
    cursor.style.left = e.pageX + "px";
  });
}

if(!Utils.isMobile()) circularCursor();
