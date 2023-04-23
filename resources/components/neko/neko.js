// TODO:
// Fix walking animations
// Go to a specific place to sleep
// Sit next to the left or right of the cursor, whichever is closer
// Sit next to/on/around links if they are hovered

const neko = document.querySelector('.neko');
const nekoUrl = "/components/neko/assets/";

const Neko = {
  awake: [nekoUrl + "awake.png", nekoUrl + "awake.png"],
  yawn: [nekoUrl + "yawn1.png", nekoUrl + "yawn2.png"],
  scratch: [nekoUrl + "scratch1.png", nekoUrl + "scratch2.png"],
  wash: [nekoUrl + "wash1.png", nekoUrl + "wash2.png"],
  sleep: [nekoUrl + "sleep1.png", nekoUrl + "sleep2.png"],
  up: [nekoUrl + "up1.png", nekoUrl + "up2.png"],
  down: [nekoUrl + "down1.png", nekoUrl + "down2.png"],
  left: [nekoUrl + "left1.png", nekoUrl + "left2.png"],
  right: [nekoUrl + "right1.png", nekoUrl + "right2.png"],
  upLeft: [nekoUrl + "upleft1.png", nekoUrl + "upleft2.png"],
  upRight: [nekoUrl + "upright1.png", nekoUrl + "upright2.png"],
  downLeft: [nekoUrl + "downleft1.png", nekoUrl + "downleft2.png"],
  downRight: [nekoUrl + "downright1.png", nekoUrl + "downright2.png"],
}

const idleLevelKeys = [
  "awake",
  "yawn",
  "awake",
  "scratch",
  "wash",
  "awake",
  "yawn",
  "sleep",
]

const cursorTolerance = 5;
const nekoStepLength = 15;
const animationSpeed = 200;
const motionPollInterval = animationSpeed;
const lastMoveInterval = animationSpeed * 4;

const rect = neko.getBoundingClientRect();

let currentX = document.body.clientWidth - 50;
let currentY = document.body.clientHeight - 50;

const distanceFromCursorX = -30;
const distanceFromCursorY = 10;

function createNekoImage() {
    var img = document.createElement('img');

  img.setAttribute("src", Neko.awake[0]);
  img.setAttribute("width", "32px");
  img.setAttribute("height", "32px");
  img.setAttribute("alt", "🐱");

  return img;
}

function timestamp() {
  return new Date().getTime();
}

const nekoImage = createNekoImage();
let currentAnimation = Neko.awake;
let lastMoved = timestamp();


function animate(first=true) {
  if (currentAnimation) {
    nekoImage.setAttribute("src", currentAnimation[first ? 0 : 1]);
  }

  setTimeout(() => animate(!first), animationSpeed);
}

function setAnimation(anim) {
  currentAnimation = Neko[anim];
}

function setIdleness(level) {
  console.log("set idleness", level);
  setAnimation(level);
}

const Direction = {
  left: "left",
  right: "right",
  up: "up",
  down: "down",
  downLeft: "downLeft",
  downRight: "downRight",
  upLeft: "upLeft",
  upRight: "upRight",
  awake: "awake",
};


function determineDirection(x, y) {
  console.log('determine direction', x, y);
  if (x === 0) {
    if (y === 0) {
      return Direction.awake;
    } else if (y > 0) {
      return Direction.down;
    } else {
      return Direction.up;
    }
  } else {
    if (y === 0) {
      if (x > 0) {
        return Direction.right;
      } else {
        return Direction.left;
      }
    } else {
      if (x > 0) {
        if (mouseX > mouseY + mouseTolerance) {
          return Direction.downRight;
        } else if (mouseX < mouseY + mouseTolerance) {
          return Direction.upRight;
        } else {
          return Direction.right;
        }
      } else {
        if (mouseY > mouseX + mouseTolerance) {
          return Direction.downLeft;
        } else if (mouseY < mouseX + mouseTolerance) {
          return Direction.upLeft;
        } else {
          return Direction.left;
        }
      }
    }
  }
}

function setDirection(direction) {
  console.log("set direction", direction);
  setAnimation(direction);
}

let mouseX = 0;
let mouseY = 0;

document.addEventListener('mousemove', function(e) {
  mouseX = e.clientX;
  mouseY = e.clientY;

  lastMoved = timestamp();
});


function moveInX() {
  if ((mouseX - currentX - distanceFromCursorX) > 0) {
    return nekoStepLength;
  } else if ((mouseX - currentX - distanceFromCursorX) < 0) {
    return -nekoStepLength;
  } else {
    return 0;
  }
}

function moveInY() {
  if ((mouseY - currentY - distanceFromCursorY) > 0) {
    return nekoStepLength;
  } else if ((mouseY - currentY - distanceFromCursorY) < 0) {
    return -nekoStepLength;
  } else {
    return 0;
  }
}

let idleLevel = 0;

function moveNeko() {
  const x = moveInX();
  const y = moveInY();
  if (idleLevel === 0 && (x || y)) {
    currentX += x;
    currentY += y;

    lastMoved = timestamp();

    setDirection(determineDirection(x, y));

    neko.style.left = currentX + 'px';
    neko.style.top = currentY + 'px';
  }

  setTimeout(() => moveNeko(), motionPollInterval);
}



function trackIdleness() {
  const curTime = timestamp();
  if (curTime - lastMoved > lastMoveInterval) {
    if (idleLevel < idleLevelKeys.length - 1) {
      idleLevel++;
    }
  } else {
    idleLevel = 0;
  }

  setIdleness(idleLevelKeys[idleLevel]);
  setTimeout(trackIdleness, lastMoveInterval);
}

neko.appendChild(nekoImage);

trackIdleness();
moveNeko();
animate();
