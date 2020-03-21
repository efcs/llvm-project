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
var source_path = path.join(process.env.GITHUB_WORKSPACE);
var build_path = path.join(process.env.GITHUB_WORKSPACE, process.env.INPUT_BUILD);
var lit_path = path.join(build_path, 'bin', 'llvm-lit');
var test_suite_path = path.join(source_path, process.env.INPUT_TEST_SUITE);

var lit_args = '-sv --shuffle ';
lit_args += ' --show-unsupported  --show-xfail --xunit-xml-output /tmp/output.xml ';
lit_args += ''.concat(' ', '--param=enable_modules=', process.env.INPUT_ENABLE_MODULES);
if (process.env.INPUT_LIT_ARGS) {
  lit_args += ' ';
  lit_args += process.env.INPUT_LIT_ARGS;
}

cmd = lit_path + ' ' + lit_args + ' ' + test_suite_path;
console.log(`${cmd}`);

p = run_command_async(cmd);
p.on('exit', (code, signal) => {
  if (code || signal) {
    handle_errors(code, signal);
  }
});
