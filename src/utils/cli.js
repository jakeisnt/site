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
  constructor() {
    this.options = [];
  }

  option(name) {
    const option = new Option(name, this);
    this.options.push(option);
    return option;
  }

  exec(args) {
    const option = this.options.find(o => o.name === args[0]);
    if (option) {
      option.apply(args.slice(1));
    } else {
      console.log('unknown option');
    }
  }
}

function cli() {
  return new CLI();
}

export { cli };
