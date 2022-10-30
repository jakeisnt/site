// Script to display the last few are.na blocks a user has seen

// deferred script
const per_page = 10;
const jakeisntArenaId = 210234;

//   `https:api.are.na/v2/channels/${jakeisntArenaId}/contents?page=${current_page}&perPage=${per_page}&direction=desc&sort=position`
// "scientific-progress-first-and-forever"

// Get the last three blocks I've posted on are.na
async function getUserBlocks() {
  // `https://api.are.na/v2/channels/${jakeisntArenaId}/contents?page=${current_page}&perPage=${per_page}&direction=desc&sort=position`
  let response = await fetch(
    `https://api.are.na/v2/users/${jakeisntArenaId}/channel?page=0&perPage=${per_page}`
  );
  let data = await response.json();
  return data.blocks;
}

// create a tag from an are.na block
function createImageTag(block) {
  var img = document.createElement('img');
  var linkTag = document.createElement('a');

  linkTag.className = 'now-playing loaded';
  linkTag.href = `https://www.are.na/block/${block.id}`;
  linkTag.target = "blank";

  img.setAttribute("src", block.image.original.url);
  linkTag.appendChild(img);

  return linkTag;
}

// Add an image to the document
async function addArenaImages() {
  let blocks = await getUserBlocks();
  for (let i = 0; i < blocks.length; i++) {
    if (blocks[i].image) {
      let divTag = createImageTag(blocks[i]);
      parentRow = document.getElementById("site-body");
      // parentRow = (i % 2 === 0 ? document.getElementById('row') : document.getElementById('row1'));
      parentRow.appendChild(divTag);
    }
  }
}
