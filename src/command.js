const runCommand = (command, args) => {
  const child = spawn(command, args, { stdio: 'inherit' });
  return new Promise((resolve, reject) => {
    child.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`${command} ${args.join(' ')} failed`));
        return;
      }
      resolve();
    });
  });
};

export { runCommand };
