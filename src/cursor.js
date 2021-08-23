let mousedOverCircle = false;
let circleX = 0;
let circleY = 0;
let circleWidth = 0;
let circleHeight = 0;

function focusElem(elem) {
  const rect = elem.getBoundingClientRect();
  const xPos = (rect.right + rect.left) / 2;
  const yPos = (rect.bottom + rect.top) / 2;
  const height = rect.bottom - rect.top;
  const width = rect.right - rect.left;
  mousedOverCircle = true;
  circleX = xPos;
  circleY = yPos;
  circleWidth = width;
  circleHeight = height;
}

function unfocusElem() {
  mousedOverCircle = false;
}

// circle that keeps expanding after mouseover until it colors the foreground!!!
function circularCursor() {
  const defaultWidth = 50;
  const defaultHeight = 50;

  const styles = `
      #invertedcursor {
        position: absolute;
        width: ${defaultWidth}px;
        height: ${defaultHeight}px;
        background: gray;
        border-radius: 50%;
        top: 0;
        left: 0;
        transform: translate(-50%, -50%);
        z-index: -100;
        transition: transform .2s;
        pointer-events: none;
      }
  `;

  UI.css(styles);
  const ball = UI.create("div", {id: "invertedcursor"})().node;

  let mouseX = 0;
  let mouseY = 0;

  let ballX = 0;
  let ballY = 0;

  let ballWidth = defaultWidth;
  let ballHeight = defaultHeight;

  const moveSpeed = 0.08;
  const growDecSpeed = 0.02;

  function capMax(val, max) {
    return val > max ? max : val;
  }

  function capMin(val, max) {
    return val < max ? max : val;
  }

  // should finish growing right when the ball gets to the destination!
  function followCursor() {
    let distX = (mousedOverCircle ? circleX : mouseX) - ballX;
    let distY = (mousedOverCircle ? circleY : mouseY) - ballY;

    ballX += distX * moveSpeed;
    ballY += distY * moveSpeed;

    ball.style.left = ballX + "px";
    ball.style.top = ballY + "px";

    if(mousedOverCircle) { // if moused over, change size to enclose elem
      const maxWidth = circleWidth * 1.2;
      const maxHeight = circleHeight * 1.2;

      const growSpeedX = Math.abs(moveSpeed / distX * 5);
      const growSpeedY = Math.abs(moveSpeed / distY * 5);

      if(ballWidth !== maxWidth) {
        ballWidth = ballWidth < maxWidth
          ? capMax(ballWidth + maxWidth * growSpeedX, maxWidth)
          : capMin(ballWidth - maxWidth * growSpeedX, circleWidth)

        ballWidth = capMax(ballWidth + maxWidth * growSpeedX, maxWidth);
        ball.style.width = ballWidth + "px";
      }
      if(ballHeight !== maxHeight) {
        ballHeight = ballHeight < maxHeight
          ? capMax(ballHeight + maxHeight * growSpeedY, maxHeight)
          : capMin(ballHeight - maxHeight * growSpeedY, maxHeight)

        ball.style.height = ballHeight + "px";
      }
    } else { // if mouse moves off, change size to usual
      if(ballWidth !== defaultWidth) {
        ballWidth = ballWidth > defaultWidth
          ? capMin(ballWidth - circleWidth * growDecSpeed, defaultWidth)
          : capMax(ballWidth + circleWidth * growDecSpeed, defaultWidth);

        ball.style.width = ballWidth + "px";
      }
      if(ballHeight !== defaultHeight) {
        ballHeight = ballHeight > defaultHeight
          ? capMin(ballHeight - circleHeight * growDecSpeed, defaultHeight)
          : capMax(ballHeight + circleHeight * growDecSpeed, defaultHeight)

        ball.style.height = ballHeight + "px";
      }
    }

    requestAnimationFrame(followCursor);
  }

  window.addEventListener("mousemove", e => {
    mouseX = event.pageX;
    mouseY = event.pageY;
  });

  followCursor();
}

const Hoverable = UI.mixin({
    events: {
      mouseenter: (e, elem) => focusElem(elem),
      mouseleave: unfocusElem
    },
})

// circle type logic
// heavily inspired by https://github.com/peterhry/CircleType
// const radiansPerDegree = Math.PI / 180;
// function degreesToRadians(deg) { return deg * radiansPerDegree; }

// function getLetterRotation(metrics, r) {
//   return metrics.reduce((data, { width }) => {
//     const rotation = radiansToDegrees(width / r);

//     return {
//       theta: data.theta + rotation,
//       rotations: data.rotations.concat([data.theta + (rotation / 2)]),
//     };
//   }, { theta: 0, rotations: [] });
// }

// function CircleType() {
//   const { fontSize, lineHeight } = window.getComputedStyle(this.element);
//   const letters = null; // TODO: get all of the letters!

//   function layout(radius, dir, lineHeight) {
//     const origin = `center ${originY / fontSize}em`;
//     const innerRadius - radius - lineHeight;

//   }

//   UI.create('div');
// }

function CircleType() {

// <svg width="200" height="200">
// <defs>
//     <path id="textPath" d="M10 50 C10 0 90 0 90 50"/>
// </defs>
// <text fill="red">
//     <textPath xlink:href="#textPath">Text on a Path</textPath>
// </text>
//     </svg>
  const imgText = `
<svg width="300" height="200">
    <rect width="100%" height="100%" fill="green" />
</svg>
  `;

  // svg.setAttribute("width", 100);
  // svg.setAttribute("height", 100);

  // const rect = document.create

  // const defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
  // const svg = document.createElementNS("http://www.w3.org/2000/svg", "defs");

  // const elem = UI.create('div', {id: "arst"})();
  // elem.innerHtml = imgText;

  UI.svg("svg",
    {width: 100, height: 100},
    UI.svg("rect", { width: "100%", height: "100%", fill: "green" })
  )();

    // '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><circle cx="40" cy="40" r="40" stroke="red" stroke-width="4" fill="blue" /></svg>';
}

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
  Hoverable(UI.create("div", { id: "mouseover-circle" }))();
}

function linkListener() {
  Array.prototype.slice.call(document.getElementsByTagName('a'))
    .forEach(link => {
      link.addEventListener("mouseenter", e => focusElem(link));
      link.addEventListener("mouseleave", (e) => { mousedOverCircle = false; });
  })
}

linkListener();
mouseOverCircle();
if(!Utils.isMobile()) circularCursor();
CircleType();
