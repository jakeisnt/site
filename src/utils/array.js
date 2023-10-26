// utilities for working with arrays


// partition an array into two parts:
// - all of the values that pass the predicate
// - all of the values that fail the predicate
function partition(array, isValid) {
  return array.reduce(([pass, fail], elem) => {
    return isValid(elem) ? [[...pass, elem], fail] : [pass, [...fail, elem]];
  }, [[], []]);
}

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
