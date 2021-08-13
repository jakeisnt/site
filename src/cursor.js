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
  console.log(width);
  console.log(height);
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
  const styles = `
      #invertedcursor {
        position: absolute;
        width: 50px;
        height: 50px;
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
  const ball = UI.create("div", {id: "invertedcursor"})();

  let mouseX = 0;
  let mouseY = 0;

  let ballX = 0;
  let ballY = 0;

  const defaultWidth = 50;
  const defaultHeight = 50;

  let ballWidth = defaultWidth;
  let ballHeight = defaultHeight;

  const moveSpeed = 0.08;
  const growDecSpeed = 0.02;

  // should finish growing right when the ball gets to the destination!

  function followCursor() {
    let distX = (mousedOverCircle ? circleX : mouseX) - ballX;
    let distY = (mousedOverCircle ? circleY : mouseY) - ballY;

    ballX = ballX + (distX * moveSpeed);
    ballY = ballY + (distY * moveSpeed);

    const growSpeedX = moveSpeed / Math.abs(distX) * 5;
    const growSpeedY = moveSpeed / Math.abs(distY) * 5;

    ball.style.left = ballX + "px";
    ball.style.top = ballY + "px";

    if(mousedOverCircle) {
      const maxWidth = circleWidth + 20;
      const maxHeight = circleHeight + 20;

      if(ballWidth < maxWidth) {
        ballWidth += maxWidth * growSpeedX;
        ball.style.width = ballWidth + "px";
      }
      if(ballHeight < maxHeight) {
        ballHeight += maxHeight * growSpeedY;
        ball.style.height = ballHeight + "px";
      }
    } else {
      if(ballWidth > defaultWidth) {
        ballWidth -= (circleWidth * growDecSpeed);
        ball.style.width = ballWidth + "px";
      }
      if(ballHeight > defaultHeight) {
        ballHeight -= (circleHeight * growDecSpeed);
        ball.style.height = ballHeight + "px";
      }
    }

    requestAnimationFrame(followCursor);
  }

  followCursor();

  window.addEventListener("mousemove", (e) => {
    mouseX = event.pageX;
    mouseY = event.pageY;
  });
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
  const circ = UI.create("div", {
    id: "mouseover-circle",
    events: {
      mouseenter: (e) => focusElem(circ),
      mouseleave: unfocusElem
    },
  }
  )();
}

function linkListener() {
  let link = document.getElementById("neulink");
  link.addEventListener("mouseenter", e => focusElem(link));
  link.addEventListener("mouseleave", (e) => { mousedOverCircle = false; });
}

linkListener();
mouseOverCircle();
if(!Utils.isMobile()) circularCursor();
