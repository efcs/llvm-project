// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

export namespace std {
#if _LIBCPP_STD_VER >= 26
  namespace contracts {
    using std::contracts::assertion_kind;
    using std::contracts::contract_violation;
    using std::contracts::detection_mode;
    using std::contracts::evaluation_semantic;
    using std::contracts::invoke_default_contract_violation_handler;
  } // namespace contracts
#endif // _LIBCPP_STD_VER >= 26
} // namespace std
