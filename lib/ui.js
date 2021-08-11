function create(name, attrs, parent) {
  let elem = document.createElement(name);
  elem = { ... elem, ...attrs };
  (parent ?? document.body).appendChild(elem);
  return elem;
}

module.exports = { create };
