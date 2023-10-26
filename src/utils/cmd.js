// command execution made stupid simple

import util from 'util';
const exec = util.promisify(require('child_process').exec);
const execSync = require('child_process').execSync;

export { exec, execSync };
