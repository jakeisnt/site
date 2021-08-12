// final goal:
// mimic how this one works! https://mtg-interieur.fr/
// - small circle leading large cursor
// - maybe inversion of text under? maybe covering the text?
// - 'absorbed' into larger bubbles that react when about to click on them!

const styles = `
    #invertedcursor {
      position: absolute;
      width: 50px;
      height: 50px;
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

let mousedOverCircle = false;
let circleX = null;
let circleY = null;

// enable controls that allow disabling css rules across the document: eg disable all shadows on the page
// (tracks a 'virtual' index of css rules, then uses these to determine what real css rules to remove, where, and when)
function mouseOverCircle() {
  const mouseover = `
    #mouseover-circle {
      position: fixed;
      width: 80px;
      height: 80px;
      background: #000;
      border-radius: 50%;
      bottom: 20px;
      right: 20px;
      box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
      z-index: 5;
    }
    `;


  UI.css(mouseover);
  const circ = UI.create("div", {
    id: "mouseover-circle",
    events: {
      mouseover: (e) => {
        const rect = circ.getBoundingClientRect();
        const xPos = (rect.right + rect.left) / 2;
        const yPos = (rect.bottom + rect.top) / 2;
        mousedOverCircle = true;
        circleX = xPos;
        circleY = yPos;
      },
      mouseleave: e => { mousedOverCircle = false; }
    },
  }
  )();
}

// just in case i want to register and manage these myself in some way
function addListener(name, callback) {
  window.addEventListener(name, callback);
}

// if the cursor is over something we can mouse over:
// - grow the ball following the cursor
// - make it hide behind the ui element
// or: make it go behind the ui element, then expand to show right behind it!
// when our cursor leaves:
// - reverse the process

function circularCursor() {
  UI.css(styles);
  const ball = UI.create("div", {id: "invertedcursor"})();
  UI.create("div", {id: "funcircle"})();

  let mouseX = 0;
  let mouseY = 0;

  let ballX = 0;
  let ballY = 0;

  const speed = 0.04;

  function followCursor() {
    let distX = (mousedOverCircle ? circleX : mouseX) - ballX;
    let distY = (mousedOverCircle ? circleY : mouseY) - ballY;

    ballX = ballX + (distX * speed);
    ballY = ballY + (distY * speed);

    ball.style.left = ballX + "px";
    ball.style.top = ballY + "px";

    requestAnimationFrame(followCursor);
  }

  followCursor();


  // document and window can both listen for this event;
  // document is higher in the listener hierarchy so it triggers first,
  // though either can perform either functionality
  addListener("mousemove", (e) => {
    mouseX = event.pageX;
    mouseY = event.pageY;
  });
}

if(!Utils.isMobile()) circularCursor();
mouseOverCircle();
