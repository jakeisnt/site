function isObject(item) {
  return item instanceof Object && item.constructor !== Array;
}

export { isObject };
