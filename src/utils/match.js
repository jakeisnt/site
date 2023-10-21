const case = (conditionArray) => value => {
  for ([conditionPredicate, formatValue] in conditionArray) {
    if (typeof conditionPredicate === "function") {
      if(conditionPredicate(value)) {
        return formatValue(value);
      }
    } else {
      if (conditionPredicate) {
        return formatValue(value);
      }
    }
  }

  return null;
}

export { case };
