/* External vars:
 * - mousedOverCircle
 * - circleX
 * - circleY
 * - circleWidth
 * - circleHeight
 */

const STROKE_WIDTH = 15;

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
        ctx.lineWidth = STROKE_WIDTH;
        ctx.lineCap = "circle";
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

  // get current position, touchscreen or not
  // default to touchscreen if it's available
  function getPos(e) {
    return {
      x: e.changedTouches ? parseInt(e.changedTouches[0].clientX) : e.clientX,
      y: e.changedTouches ? parseInt(e.changedTouches[0].clientY) : e.clientY
    }
  }

  // reposition the drawing coordinate
  function reposition(e) {
    const {x, y} = getPos(e);

    coordX = x - canvas.offsetLeft;
    coordY = y - canvas.offsetTop;
  }

  // start drawing if we're not mousing over anything
  function start(e) {
    if(!mousedOverCircle || isMobile()) {
      document.addEventListener("mousemove", draw);
      document.addEventListener("touchmove", draw);
      reposition(e);
    }
  }

  // stop drawing on the canvas
  function stop() {
    document.removeEventListener("mousemove", draw);
    document.removeEventListener("touchmove", draw);
  }

  document.addEventListener("mousedown", start)
  document.addEventListener("touchstart", start)
  document.addEventListener("mouseup", stop)
  document.addEventListener("touchend", stop)
  window.addEventListener("resize", resize)
}

MainCanvas()
