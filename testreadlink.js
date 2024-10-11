const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');

// // Create a temporary directory for the test
// const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'chown-test-'));
// process.chdir(tmpDir); // Move to the temp directory to keep things organized

// Paths for files
const relativeFileTarget = 'relativeFileTarget.txt';
const relativeFileSymlink = 'relativeFileSymlink.txt';

const absoluteFileTarget = path.resolve('absoluteFileTarget.txt');
const absoluteFileSymlink = 'absoluteFileSymlink.txt';

// Paths for directories
const relativeDirTarget = 'relativeDirTarget';
const relativeDirSymlink = 'relativeDirSymlink';

const absoluteDirTarget = path.resolve('absoluteDirTarget');
const absoluteDirSymlink = 'absoluteDirSymlink';

// Create target files
fs.writeFileSync(relativeFileTarget, 'This is a relative target file.');
fs.writeFileSync(absoluteFileTarget, 'This is an absolute target file.');

// Create target directories
fs.mkdirSync(relativeDirTarget);
fs.mkdirSync(absoluteDirTarget);

// Create symlinks for files
fs.symlinkSync(relativeFileTarget, relativeFileSymlink);
fs.symlinkSync(absoluteFileTarget, absoluteFileSymlink);

// Create symlinks for directories
fs.symlinkSync(relativeDirTarget, relativeDirSymlink);
fs.symlinkSync(absoluteDirTarget, absoluteDirSymlink);

// Read and log symlink paths for files
const relFileLinkPath = fs.readlinkSync(relativeFileSymlink, 'utf8');
const absFileLinkPath = fs.readlinkSync(absoluteFileSymlink, 'utf8');

console.log(`File symlink ${relativeFileSymlink} points to: ${relFileLinkPath}`); // Outputs: relativeFileTarget.txt
console.log(`File symlink ${absoluteFileSymlink} points to: ${absFileLinkPath}`); // Outputs: /full/path/to/absoluteFileTarget.txt

// Read and log symlink paths for directories
const relDirLinkPath = fs.readlinkSync(relativeDirSymlink, 'utf8');
const absDirLinkPath = fs.readlinkSync(absoluteDirSymlink, 'utf8');

console.log(`Directory symlink ${relativeDirSymlink} points to: ${relDirLinkPath}`); // Outputs: relativeDirTarget
console.log(`Directory symlink ${absoluteDirSymlink} points to: ${absDirLinkPath}`); // Outputs: /full/path/to/absoluteDirTarget

// Clean up files (optional)
fs.unlinkSync(relativeFileSymlink);
fs.unlinkSync(absoluteFileSymlink);
fs.unlinkSync(relativeFileTarget);
fs.unlinkSync(absoluteFileTarget);

// Clean up directories (optional)
fs.unlinkSync(relativeDirSymlink);
fs.unlinkSync(absoluteDirSymlink);
fs.rmdirSync(relativeDirTarget);
fs.rmdirSync(absoluteDirTarget);
