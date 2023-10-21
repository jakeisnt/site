// command execution made stupid simple
// this might need more stuff... not sure yet.

import util from 'util';
const exec = util.promisify(require('child_process').exec);

// exec a command synchronously
// const execSync = (command, args) => {
//   const child = spawn(command, args, { stdio: 'inherit' });
//   return new Promise((resolve, reject) => {
//     child.on('close', (code) => {
//       if (code !== 0) {
//         reject(new Error(`${command} ${args.join(' ')} failed`));
//         return;
//       }
//       resolve();
//     });
//   });
// };

// exec a command asynchronously
// const exec = (command, args) => {
//   const child = spawn(command, args, { stdio: 'inherit' });
//   return new Promise((resolve, reject) => {
//     child.on('close', (code) => {
//       if (code !== 0) {
//         reject(new Error(`${command} ${args.join(' ')} failed`));
//         return;
//       }
//       resolve();
//     });
//   });
// };

export { exec };
