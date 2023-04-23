// 1. component that follows mouse
// 2. integrate images
// 3. animate image

const neko = document.querySelector('.neko');
const nekoComponents = "/components/neko/assets/";

const speed = 0.1;

let currentX = 0;
let currentY = 0;

function createNekoImage() {
    var img = document.createElement('img');

    img.setAttribute("src", nekoComponents + "awake.png");
    img.setAttribute("width", "32px");
    img.setAttribute("height", "32px");
    img.setAttribute("alt", "🐱");

    return img;
}

const nekoImage = createNekoImage();

document.addEventListener('mousemove', function(e) {
  let body = document.querySelector('body');

  currentX += (e.clientX - currentX) * speed;
  currentY += (e.clientY - currentY) * speed;
  neko.style.left = currentX + 'px';
  neko.style.top = currentY + 'px';
});

neko.appendChild(nekoImage);
