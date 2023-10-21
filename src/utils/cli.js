// utilities for creating cli programs
import { makeSpaces } from './string';

// api:
// cli('name')
//   .describe('description')
//   .option(name).describe(desc).action(fn)
//   .option(name).describe(desc).action(fn)
//   .exec(argv)

// bold a string when printing it out
function bold(str) {
  return '\x1b[1m' + str + '\x1b[0m';
}

class Option {
  constructor(name, creator) {
    this.name = name;
    this.description = '';
    this.apply = () => {};
    this.creator = creator;
  }

  describe(desc) {
    this.description = desc;
    return this;
  }

  action(fn) {
    this.apply = fn;
    return this;
  }

  option(name) {
    return this.creator.option(name);
  }

  exec(args) {
    return this.creator.exec(args);
  }
}

class CLI {
  name = '';
  options = [];
  description = null;

  constructor(name = '<command>') {
    this.name = name;
    this.options = [];
  }

  option(name) {
    const option = new Option(name, this);
    this.options.push(option);
    return option;
  }

  describe(desc) {
    this.description = desc;
    return this;
  }

  exec(args) {
    this.addHelpOption();
    const option = this.options.find(o => o.name === args[0]);

    if (option) {
      option.apply(args.slice(1));
    } else {
      this.printHelp();
    }
  }

  // add the help option
  // the help option needs to reference `this`,
  // so it can't be added in the constructor
  addHelpOption() {
    this.option('help')
      .describe('Show help')
      .action(() => this.printHelp());

    return this;
  }

  printHelp() {
    console.log(`${bold(this.name)}${this.description ? `: ${this.description}` : ''}`);
    console.log(``);
    const maxNameLength = this.options.reduce((max, o) => Math.max(max, o.name.length), 0);

    this.options.forEach(o => {
      const spaces = makeSpaces(maxNameLength - o.name.length);
      console.log(`  ${bold(o.name)} ${spaces} ${o.description}`);
    });

    console.log(``);
  }
}

function cli(...args) {
  return new CLI(...args);
}

export { cli };
