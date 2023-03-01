// https://stackoverflow.com/questions/260857/changing-website-favicon-dynamically
function changeFavicon(text) {
  const canvas = document.createElement('canvas');
  canvas.height = 64;
  canvas.width = 64;
  const ctx = canvas.getContext('2d');
  ctx.font = '64px serif';
  ctx.fillText(text, 0, 64);

  const link = document.createElement('link');
  const oldLinks = document.querySelectorAll('link[rel="shortcut icon"]');
  oldLinks.forEach(e => e.parentNode.removeChild(e));
  link.id = 'dynamic-favicon';
  link.rel = 'shortcut icon';
  link.href = canvas.toDataURL();
  document.head.appendChild(link);
}

// changeFavicon('❤');
