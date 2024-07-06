// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


void test_pre_parse(int x) pre(x != 0);

