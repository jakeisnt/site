const cond = (conditionArray) => value => {
  for (const [conditionPredicate, formatValue] in conditionArray) {
    const valueFormatter = (typeof formatValue === 'function') ? formatValue : () => formatValue;

    if (typeof conditionPredicate === "function") {
      if(conditionPredicate(value)) {
        return valueFormatter(value);
      }
    } else {
      if (conditionPredicate) {
        return valueFormatter(value);
      }
    }
  }

  return null;
}

export { cond };
