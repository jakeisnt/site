const util = require('util');
const exec = util.promisify(require('child_process').exec);
const path = require('path');

async function log(file, sourceDir) {
  const filePath = path.join(sourceDir, file);
  const command = `git log --all --full-history --pretty="format:%h %H %ad" --date default --date=format:'%Y-%m-%d' ${filePath}`;
  try {
    const { stdout } = await exec(command);
    if (stdout) {
      return stdout.split('\n').map((line) => {
        const [shortHash, longHash, commitDate] = line.split(' ');
        return {
          shortHash,
          longHash,
          commitDate,
          file: file,
        };
      });
    } else {
      throw new Error(`git log command failed on path ${file}`);
    }
  } catch (error) {
    throw error;
  }
}

async function lastLog(file, sourceDir) {
  const filePath = path.join(sourceDir, file);
  const command = `git log -1 --full-history --pretty="format:%h %H %ad %ct" --date default --date=format:'%Y-%m-%d' ${filePath}`;
  try {
    const { stdout } = await exec(command);
    if (stdout) {
      const [line] = stdout.split('\n');
      const [shortHash, longHash, commitDate, timestamp] = line.split(' ');
      return {
        shortHash,
        longHash,
        commitDate,
        timestamp: parseInt(timestamp, 10),
        file: file,
      };
    } else {
      throw new Error(`git log command failed on path ${file}`);
    }
  } catch (error) {
    throw error;
  }
}

async function lastTimestamp(sourceDir, path) {
  const filePath = path ? path : '';
  const command = `git log -1 --pretty=format:%ct --follow -- ${filePath}`;
  try {
    const { stdout } = await exec(command, { cwd: sourceDir });
    return parseInt(stdout, 10);
  } catch (error) {
    throw error;
  }
}

function historyLink(longHash, filePath) {
  return `${const.sourceUrl}/blob/${longHash}/${filePath}`;
}

async function checkout(sourceDir, branch) {
  await exec(`git checkout ${branch}`, { cwd: sourceDir });
}

async function addAll(sourceDir) {
  await exec('git add .', { cwd: sourceDir });
}

async function commit(sourceDir) {
  await exec('git -c commit.gpgsign=false commit -m "robot commit"', { cwd: sourceDir });
}

async function push(sourceDir) {
  await exec('git push', { cwd: sourceDir });
}

async function removeUntracked(dir) {
  await exec('git clean -fxd', { cwd: dir });
}

async function currentBranch(dir) {
  const { stdout } = await exec('git branch --show-current', { cwd: dir });
  return stdout.trim();
}

async function status(dir) {
  const { stdout } = await exec('git status', { cwd: dir });
  console.log(stdout);
}

async function stash(dir) {
  await exec('git stash', { cwd: dir });
}

async function stashPop(dir) {
  await exec('git stash pop', { cwd: dir });
}

module.exports = {
  log,
  lastLog,
  lastTimestamp,
  historyLink,
  checkout,
  addAll,
  commit,
  push,
  removeUntracked,
  currentBranch,
  status,
  stash,
  stashPop,
};
