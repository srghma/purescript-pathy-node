const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

// test that I cannot use cpSync on directories without recursive flag

// Create a temporary directory and a subdirectory inside it
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'test-dir-'));
const subDir = path.join(tmpDir, 'subdir');

// Create the subdirectory
fs.mkdirSync(subDir);

// Define a target directory where we will try to copy `subdir`
const targetDir = path.join(os.tmpdir(), 'test-dir-copy');

// Try copying the subdirectory without the recursive flag
let myerror = null
try {
  fs.cpSync(subDir, targetDir);
} catch (err) {
  myerror = err
}

// Error: Recursive option not enabled, cannot copy a directory: /tmp/test-dir-C7Svkd/subdir
//     at cpSyncFn (node:internal/fs/cp/cp-sync:56:13)
//     at Object.cpSync (node:fs:3046:3)
//     at REPL23:2:6
//     at ContextifyScript.runInThisContext (node:vm:137:12)
//     at REPLServer.defaultEval (node:repl:598:22)
//     at bound (node:domain:432:15)
//     at REPLServer.runBound [as eval] (node:domain:443:12)
//     at REPLServer.onLine (node:repl:927:10)
//     at REPLServer.emit (node:events:532:35)
//     at REPLServer.emit (node:domain:488:12) {
//   code: 'ERR_FS_EISDIR'
// }
