const setProperty = (obj, key, value) => {
  Object.defineProperty(obj, key, {
    value,
    writable: true,
    enumerable: true,
    configurable: true
  });
}

// clone a class instance,
// overriding properties as specified by the optional argument
const cloneClassInstance = (ins, ...propertiesToOverride) => {
  try {
    let newIns = Object.create(Object.getPrototypeOf(ins));
    newIns = Object.assign(newIns, ins);
    // propertiesToOverride?.forEach(([key, val]) => {
    //   setProperty(newIns, key, val)
    // });
    return newIns;

  } catch (err) {
    console.error(err);
    return null;
  }
}

export { cloneClassInstance, setProperty };
