// generate a string of `numSpaces` spaces
const makeSpaces = (numSpaces: number) => {
  let spaces = "";
  for (let i = 0; i < numSpaces; i++) {
    spaces += " ";
  }
  return spaces;
};

export { makeSpaces };
