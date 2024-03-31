// styles things that print out to the cli

enum TerminalColors {
  red = 31,
  green = 32,
  yellow = 33,
  blue = 34,
  magenta = 35,
  cyan = 36,
  grey = 90,
}

enum KeyCodes {
  ESC = "\u001B[",
  OSC = "\u001B]",
  BEL = "\u0007",
  SEP = ";",
}

/**
 * Generate link text in the terminal.
 * @param text
 * @param url
 * @returns
 */
const linkText = (text: string, url: string) =>
  [
    KeyCodes.OSC,
    "8",
    KeyCodes.SEP,
    KeyCodes.SEP,
    url,
    KeyCodes.BEL,
    text,
    KeyCodes.OSC,
    "8",
    KeyCodes.SEP,
    KeyCodes.SEP,
    KeyCodes.BEL,
  ].join("");

const underlineText = (text: string) => {
  return `\u001b[4m${text}\u001b[24m`;
};

function boldText(text: string) {
  return `\x1b[1m${text}\x1b[0m`;
}

// change the color of the text when printing it out
function colorText(str: string, color: TerminalColors) {
  return "\x1b[" + TerminalColors[color] + "m" + str + "\x1b[0m";
}

class PrintStyle {
  isBold = false;
  isItalic = false;
  isUnderlined = false;
  colorName = null;
  text = "";
  url = null;

  constructor(text) {
    this.text = text;
  }

  // bold a string when printing it out
  bold() {
    this.isBold = true;
    return this;
  }

  /**
   * add a link with a url.
   * if a url is not provided, assume the text is also the url.
   */
  link(maybeUrl?: string) {
    if (maybeUrl) {
      this.url = maybeUrl;
    } else {
      this.url = this.text;
    }
    return this;
  }

  color(colorName) {
    if (!(colorName in TerminalColors)) {
      throw new Error(
        `Tried to color a terminal value. ${colorName} is not a valid color`
      );
    }

    this.colorName = colorName;
    return this;
  }

  underline() {
    this.isUnderlined = true;
    return this;
  }

  /**
   * Redefine the default toString behavior to pretty-print.
   * @returns
   */
  toString() {
    let str = this.text;
    if (this.url) {
      str = linkText(str, this.url);
    }
    if (this.isBold) {
      str = boldText(str);
    }
    if (this.colorName) {
      str = colorText(str, this.colorName);
    }

    if (this.isUnderlined) {
      str = underlineText(str);
    }

    return str;
  }
}

function style(text: string) {
  return new PrintStyle(text);
}

function bold(text: string) {
  return new PrintStyle(text).bold();
}

function color(text: string, colorName) {
  return new PrintStyle(text).color(colorName);
}

/**
 * Accepts a string and a url. Prints a link.
 * If no url is provided, the text is assumed to be the url.
 */
function link(text: string, url?: string) {
  return new PrintStyle(text).link(url);
}

/**
 * Prints an underlined version of the text.
 */
function underline(text: string) {
  return new PrintStyle(text).underline();
}

export { style, bold, color, link, underline };
