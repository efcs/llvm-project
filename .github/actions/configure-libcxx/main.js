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
var cache_path = path.join(process.env.GITHUB_WORKSPACE, process.env.INPUT_CMAKE_CACHE);

fs.mkdirSync(build_path, { recursive : true });

var cmake_args = "";

if (process.env.INPUT_EXTRA_CMAKE_ARGS) {
  cmake_args += process.env.INPUT_EXTRA_CMAKE_ARGS;
}

cmd = 'cmake ' + ' -GNinja ' + ' -S ' + source_path + ' -B ' + build_path;
cmd += ' -C ' + cache_path + ' ' + cmake_args;
console.log(`${cmd}`);

p = run_command_async(cmd);
p.on('exit', (code, signal) => {
  if (code || signal) {
    handle_errors(code, signal);
  }
  p = run_command_async('ninja -v -C ' + build_path + ' projects/libcxx/all projects/libcxxabi/all');
  p.on('exit', (code, signal) => {
    handle_errors(code, signal);
  });
});
