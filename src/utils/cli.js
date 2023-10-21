// utilities for creating cli programs

// api:
//   option(name).description(desc).action(fn)

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

  constructor(name = '<command>') {
    this.name = name;
    this.options = [];
  }

  option(name) {
    const option = new Option(name, this);
    this.options.push(option);
    return option;
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
    console.log(`usage: ${this.name} [options]`);

    this.options.forEach(o => {
      console.log(`  ${o.name} - ${o.description}`);
    });
  }
}

function cli(...args) {
  return new CLI(...args);
}

export { cli };
