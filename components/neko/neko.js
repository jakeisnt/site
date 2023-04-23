// TODO:
// Introduce walking animations
// Animate waking up with yawn?
// Go to a specific place to sleep?
// Sit next to the left or right of the cursor, whichever is closer

const neko = document.querySelector('.neko');
const nekoUrl = "/components/neko/assets/";

const Neko = {
  awake: [nekoUrl + "awake.png", nekoUrl + "awake.png"],
  yawn: [nekoUrl + "yawn1.png", nekoUrl + "yawn2.png"],
  scratch: [nekoUrl + "scratch1.png", nekoUrl + "scratch2.png"],
  wash: [nekoUrl + "wash1.png", nekoUrl + "wash2.png"],
  sleep: [nekoUrl + "sleep1.png", nekoUrl + "sleep2.png"],
}

const nekoStepLength = 1;
const motionPollInterval = 10;
const animationSpeed = 600;
const lastMoveInterval = animationSpeed * 3;

const rect = neko.getBoundingClientRect();

let currentX = document.body.clientWidth - 50;
let currentY = document.body.clientHeight - 50;

const distanceFromCursorX = -30;
const distanceFromCursorY = 10;

function createNekoImage() {
    var img = document.createElement('img');

  img.setAttribute("src", Neko.awake);
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


function animate() {
  if (currentAnimation) {
    nekoImage.setAttribute("src", currentAnimation[0]);
  }
  setTimeout(() => {
    if (currentAnimation) {
      nekoImage.setAttribute("src", currentAnimation[1]);
    }
    setTimeout(animate, animationSpeed);
  }, animationSpeed);
}

function setIdleness(level) {
  console.log("set idleness", level);
  currentAnimation = Neko[level];
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

    neko.style.left = currentX + 'px';
    neko.style.top = currentY + 'px';
  }

  setTimeout(() => moveNeko(), motionPollInterval);
}

const idleLevelKeys = [
  "awake",
  "wash",
  "awake",
  "yawn",
  "awake",
  "scratch",
  "yawn",
  "sleep",
];

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
