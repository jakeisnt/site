// 1. component that follows mouse
// 2. integrate images
// 3. animate image

const neko = document.querySelector('.neko');
const nekoUrl = "/components/neko/assets/";

const Neko = {
  awake: nekoUrl + "/awake.png",
}

const speed = 2;

const rect = neko.getBoundingClientRect();

let currentX = document.body.clientWidth - 50;
let currentY = document.body.clientHeight - 50;

const distanceFromCursorX = -10;
const distanceFromCursorY = 10;

function createNekoImage() {
    var img = document.createElement('img');

  img.setAttribute("src", Neko.awake);
  img.setAttribute("width", "32px");
  img.setAttribute("height", "32px");
  img.setAttribute("alt", "🐱");

  return img;
}


const nekoImage = createNekoImage();

function isPositive(n) {
  return n > 0;
}

function moveInX(e) {
  if (isPositive(e.clientX - currentX - distanceFromCursorX)) {
    return speed;
  } else {
    return -speed;
  }
}

function moveInY(e) {
  if (isPositive(e.clientY - currentY - distanceFromCursorY)) {
    return speed;
  } else {
    return -speed;
  }
}

document.addEventListener('mousemove', function(e) {
  let body = document.querySelector('body');

  currentX += moveInX(e);
  currentY += moveInY(e);

  neko.style.left = currentX + 'px';
  neko.style.top = currentY + 'px';
});

neko.appendChild(nekoImage);
