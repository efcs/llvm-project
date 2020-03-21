const { execSync } = require('child_process');
const { spawn, spawnSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const process = require('process');

function run_command(cmd) {
  var p;
  console.log(`${cmd}`)
  execSync(cmd, (error, stdout, stderr) => {
    console.log(`${stdout}`);
    console.error(`${stderr}`);
    if (error) {
      process.exit(error.code);
    }
  });
  return null;
}

function run_command_async(cmd) {
  p = spawn(cmd, { shell : true});

  p.stdout.on('data', (data) => {
    process.stdout.write(data.toString());
  });

  p.stderr.on('data', (data) => {
    process.stderr.write(data.toString());
  });

  p.on('error', (code) => {
    process.exit(code);
  });

  return p
}

function get_action_cmd(action) {
  return 'node ' + path.join(__dirname, '..', action, 'main.js');
}

function handle_errors(code, signal) {
  if (code) {
    process.exit(code);
  }
  if (signal) {
    console.error(`Process exited: ${signal}`);
    process.exit(1);
  }
}


if (process.argv.length != 2) {
  console.error("usage: process.argv[0] process.argv[1]");
  process.exit(1);
}


if (!process.env.INPUT_BUILD ) {
  console.error('not build specified');
  process.exit(1)
}
var source_path = path.join(process.env.GITHUB_WORKSPACE, 'llvm');
var build_path = path.join(process.env.GITHUB_WORKSPACE, process.env.INPUT_BUILD);

fs.mkdirSync(build_path, { recursive : true });
process.chdir(build_path);

const runtimes = process.env.INPUT_RUNTIMES.split(" ");
let build_targets = '';
for  (let rt in runtimes) {
  const target = ''.concat('projects/', runtimes[rt], '/all');
  console.log(target);
  build_targets += ' ';
  build_targets += target;
}

var cmake_args = "";
cmake_args += ' -DCMAKE_C_COMPILER=' + process.env.INPUT_CC;
cmake_args += ' -DCMAKE_CXX_COMPILER=' + process.env.INPUT_CXX;
cmake_args += ' -DLIBCXX_CXX_ABI=' + process.env.INPUT_CXXABI;
if (process.env.INPUT_SANITIZER) {
  cmake_args += ' -DLLVM_USE_SANITIZER=' + process.env.INPUT_SANITIZER;
}

if (process.env.INPUT_CMAKE_ARGS) {
  cmake_args += process.env.INPUT_CMAKE_ARGS;
}

cmd = 'cmake ' + ' -GNinja ' + cmake_args;
cmd += ' -DLLVM_ENABLE_PROJECTS="' + runtimes.join(";") + '"';
cmd += ' ' + cmake_args + ' ' + source_path;
console.log(`${cmd}`);

p = run_command_async(cmd);
p.on('exit', (code, signal) => {
  if (code || signal) {
    handle_errors(code, signal);
  }
  p = run_command_async('ninja -v -C ' + build_path + ' ' + build_targets);
  p.on('exit', (code, signal) => {
    handle_errors(code, signal);
  });
});
