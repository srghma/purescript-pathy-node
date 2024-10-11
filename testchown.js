const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

const uid = process.getuid(); // Current user's UID
const gid = process.getgid(); // Current user's GID

const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'chown-test-'));

const existingFile = path.join(tmpDir, 'existingFile.txt');
const existingDir = path.join(tmpDir, 'existingDir');
const existingSymlF = path.join(tmpDir, 'existingSymlinkFile');
const existingSymlD = path.join(tmpDir, 'existingSymlinkDir');

const nonExF = path.join(tmpDir, 'nonExFile.txt');
const nonExD = path.join(tmpDir, 'nonExDir');
const nonExistingSymlF = path.join(tmpDir, 'nonExistingSymlinkFile');
const nonExistingSymlD = path.join(tmpDir, 'nonExistingSymlinkDir');

// Create actual files and directories
fs.writeFileSync(existingFile, 'Test file');
fs.mkdirSync(existingDir);

// Create symlinks
fs.symlinkSync(existingFile, existingSymlF);
fs.symlinkSync(existingDir, existingSymlD);

/////

try {
  console.log('Testing chownSync...');
  fs.chownSync(existingFile, uid, gid); // Change owner of file
  fs.chownSync(existingDir, uid, gid); // Change owner of dir
} catch (err) {
  console.error('Error during chownSync:', err);
}

try {
  console.log('Testing lchownSync...');
  fs.lchownSync(existingSymlF, uid, gid); // Change owner of symlink to file
  fs.lchownSync(existingSymlD, uid, gid); // Change owner of symlink to dir
} catch (err) {
  console.error('Error during lchownSync:', err);
}

try {
  console.log('Testing fchownSync...');
  const fd = fs.openSync(existingFile, 'r'); // Open file descriptor
  fs.fchownSync(fd, uid, gid); // Change owner using file descriptor
  fs.closeSync(fd);
} catch (err) {
  console.error('Error during fchownSync:', err);
}

try {
  console.log('Testing chown on non-existing file/dir...');
  fs.chownSync(nonExF, uid, gid); // Should fail
} catch (err) {
  console.error('Error on non-existing file:', err);
}

try {
  fs.chownSync(nonExD, uid, gid); // Should fail
} catch (err) {
  console.error('Error on non-existing directory:', err);
}

try {
  fs.lchownSync(nonExistingSymlF, uid, gid); // Should fail
} catch (err) {
  console.error('Error on non-existing symlink to file:', err);
}

try {
  fs.lchownSync(nonExistingSymlD, uid, gid); // Should fail
} catch (err) {
  console.error('Error on non-existing symlink to dir:', err);
}

fs.rmSync(tmpDir, { recursive: true, force: true });
