function isArray(item) {
  return item instanceof Object && item.constructor === Array;
}

export { isArray };
