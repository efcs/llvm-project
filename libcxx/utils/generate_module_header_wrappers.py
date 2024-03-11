#!/usr/bin/env python3
import argparse
from pathlib import Path
import sys
import os
import glob

WRAP_HEADER = '''
#ifndef _LIBCPP__MODULE_WRAPPER_HEADER
#define _LIBCPP__MODULE_WRAPPER_HEADER

#include <__config>
#include <version>

import std.compat;

#endif
'''

def main():
  parser = argparse.ArgumentParser(description='Generate module header wrappers')
  parser.add_argument('--cxx-isystem', type=str, required=True, help='The path to the C++ include directory')
  parser.add_argument('--output-path', '-o', required=True, type=str, help='The path to the output directory')
  parser.add_argument('--module-name', '-m', required=False, default='std', choices=('std', 'std.compat'))
  args = parser.parse_args()
  textual_headers = [Path(p) for p in ['__config', 'cassert', 'cstdio', '__availability', 'cerrno', 'version']]
  wrapped_headers = []
  cxx_isystem = Path(args.cxx_isystem)
  assert cxx_isystem.is_dir(), f'cxx-isystem is not a directory: {cxx_isystem}'
  out_path = Path(args.output_path)
  if not out_path.exists():
    out_path.mkdir()
  assert out_path.is_dir(), f'output-path is not a directory: {out_path}'
  textual_headers = [Path(p) for p in textual_headers]
  gather_headers = cxx_isystem.glob('*')
  for f in gather_headers:
    if not f.is_file():
      continue
    if f.name.startswith('__'):
      continue
    if f.suffix == '.h':
      textual_headers += [f.name]
      continue
    if Path(f.name) not in textual_headers:
      wrapped_headers += [Path(f.name)]
      continue
  print('Textual headers:', textual_headers)
  print('Wrapped headers:', wrapped_headers)
  src_dir = cxx_isystem
  dest_dir = out_path
  for f in textual_headers:
    in_file = src_dir / f
    assert in_file.is_file(), f'File not found: {in_file}'
    out_file = dest_dir / f
    print('Copying', in_file, 'to', out_file)
    out_file.write_text(in_file.read_text())
  for f in wrapped_headers:
    out_file = dest_dir / f
    print('Wrapping', f, 'to', out_file)
    out_file.write_text(WRAP_HEADER)
  print('Done wrapping headers in directory', dest_dir)


if __name__ == '__main__':
  main()
