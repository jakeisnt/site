/* External vars:
 * - mousedOverCircle
 * - circleX
 * - circleY
 * - circleWidth
 * - circleHeight
 */

// a background canvas to draw on
function MainCanvas() {
  let coordX = 0;
  let coordY = 0;

  const canvasCSS = `
    #canvas {
      width: 100vw;
      height: 100vh;
      z-index: -999;
      position: absolute;
      top: 0;
      left: 0;
    }
  `;
  const canvas = UI.create(
    "canvas",
    { id: "canvas", events: {} }
  )().node;
  UI.css(canvasCSS);


  const ctx = canvas.getContext("2d");
  // scale to match coordinates to the window width and height
  ctx.scale(ctx.canvas.width / window.innerWidth, ctx.canvas.height / window.innerHeight)

  // draw on the canvas if we're not mousing over anything
  function draw(e) {
    if(!mousedOverCircle) {
        ctx.beginPath();
        ctx.lineWidth = defaultWidth;
        ctx.lineCap = "round";
        ctx.strokeStyle = isDarkMode() ? "red" : "#ACD3ED";
        ctx.moveTo(coordX, coordY);
        reposition(e);
        ctx.lineTo(coordX, coordY);
        ctx.stroke();
    }
  }

  // resize the canvas to the window size
  function resize() {
    ctx.canvas.width = window.innerWidth;
    ctx.canvas.height = window.innerHeight;
  }

  // reposition the drawing coordinate
  function reposition(event) {
    coordX = event.clientX - canvas.offsetLeft;
    coordY = event.clientY - canvas.offsetTop;
  }

  // start drawing if we're not mousing over anything
  function start(e) {
    if(!mousedOverCircle) {
      document.addEventListener("mousemove", draw);
      reposition(e);
    }
  }

  // stop drawing on the canvas
  function stop() {
    document.removeEventListener("mousemove", draw);
  }

  document.addEventListener("mousedown", start)
  document.addEventListener("mouseup", stop)
  window.addEventListener("resize", resize)
}

MainCanvas()
