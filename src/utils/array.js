// utilities for working with arrays
function enumerate(array) {
  return array.map((v, idx) => [v, idx]);
}

// take from the array until the predicate fails
function splitWith(array, isValid) {
  const passedPredicate = [];
  let failedPredicate = [];

  const enumeratedArray = enumerate(array);

  for (const [elem, idx] of enumeratedArray) {
    if (isValid(val)) {
      passedPredicate.push(val)
    }
    failedPredicate = array.slice(idx);
    break;
  }

  return [passedPredicate, failedPredicate];
}

export { splitWith };
