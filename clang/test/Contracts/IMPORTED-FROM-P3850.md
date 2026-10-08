# Tests imported from Joshua Berne's contracts-p3850 branch

TEMPORARY triage documentation for Eric; delete once the round-two fixes land.

Source: `josh/contracts-p3850` (notadragon/llvm-project, contracts-p3850), files under
`clang/test/Contracts` (excluding `Runnable/`) plus the contract-related files under
`clang/test/{Modules,Parser,CXX}`.  `Runnable/` and libc++ are handled separately.

Categories: A = tests P2900R14 behaviour (import); B = tests a C++29 extension / his ABI
(skip); C = same test exists in our tree; D = his `OpenBugs/`.

Counts: A=58, B=394, C=27, D=7, total=486.

Re-triaged against the round-two integration branch (eric/contracts-fixes-r2): four
OpenBugs entries (contract-unexpanded-pack, contract-dependent-predicate-temporary-ast,
postcondition-param-lambda, constify-captured-this-member) are fixed there and moved back to
the normal directories; six tests had their expectations updated to the round-two capture /
constification / constant-evaluation diagnostics (see the rows below).

Attribution: files that are verbatim (modulo RUN-line normalisation) are committed with
Joshua as author; anything whose expectations or content changed is committed by Eric with
a 'Based on tests from Joshua Berne's contracts-p3850 branch.' line.

| file (relative to clang/test) | cat | outcome |
|---|---|---|
| `Contracts/alternative-keywords.cpp` | A | imported as alternative-keywords.cpp (superset of ours, same RUN line); passes |
| `Contracts/C/assert-observe.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/combined.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contract-on-non-function.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-accessors.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-config-location.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-config-location.json` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-handler-sig-nonconst.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-handler-sig-nonptr.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-handler-sig-noparam.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-handler-sig-twoparam.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-kind-macros.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-post-order.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-post-single-eval.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-pre-ignore-illformed.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-proto-leak.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-stdcontracts-macros.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-trigger-enforced.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-trigger-observed.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/contracts-trigger-quick.c` | B | skipped: C contracts (P4299) |
| `Contracts/ce-enforce-rt-ignore.cpp` | B | skipped: per-context (constexpr vs runtime) semantics via configuration file (P3595) |
| `Contracts/ce-enforce-rt-ignore.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/ce-ignore-rt-enforce.cpp` | B | skipped: per-context (constexpr vs runtime) semantics via configuration file (P3595) |
| `Contracts/ce-ignore-rt-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/ce-observe-rt-enforce.cpp` | B | skipped: per-context (constexpr vs runtime) semantics via configuration file (P3595) |
| `Contracts/ce-observe-rt-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/C/handler-signature-bad.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/implicit-link.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/keywords-no-flag.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/multi-pre.c` | B | skipped: C contracts (P4299) |
| `Contracts/constexpr-config-parse.cpp` | B | skipped: P3595 caller-side configuration file |
| `Contracts/constexpr-config-parse.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/constexpr.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/constification.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording; his drops the structured-binding constification error and the 'this is const' notes) |
| `Contracts/constify-template-params.cpp` | C | ours kept: rename of our template-test2.cpp with only wording changes |
| `Contracts/constructor-contracts.cpp` | A | imported verbatim; passes |
| `Contracts/contract-ast-print.cpp` | A | adapted: only the P2900R14 cases kept (no P3098 captures / P4283 requires / P3099 message); passes |
| `Contracts/contract-capture-note-location.cpp` | A | imported; re-worded to our implicit-capture diagnostics; the generic-lambda case now gets the contract-capture error (outer at definition, inner at instantiation) instead of the odr-usable error; passes |
| `Contracts/contract-complete-class-late-parse.cpp` | A | adapted: virt-specifier cases dropped (R14 forbids contracts on virtual functions), requires-clause cases kept; passes |
| `Contracts/contract-constexpr-multi-observe.cpp` | B | skipped: P3850 umbrella + libc++ constexpr observe diagnostics |
| `Contracts/contract-constexpr-observe-cap.cpp` | B | skipped: P3850 umbrella + libc++ constexpr observe diagnostics |
| `Contracts/contract-constexpr-observe-masks-enforce.cpp` | B | skipped: P3850 umbrella + libc++ constexpr observe diagnostics |
| `Contracts/contract-constexpr-observe-masks-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/contract-constexpr-repeat-call.cpp` | B | skipped: P3400 labels + libc++ runtime |
| `Contracts/contract-constexpr-repeat-call-shapes.cpp` | A | adapted: constant-evaluation half kept as -fsyntax-only; the run-time half (needs libc++) is for the Runnable tests; passes |
| `Contracts/contract-constexpr-side-effect.cpp` | A | adapted: constant-evaluation half kept as -fsyntax-only; the run-time half (needs libc++) is for the Runnable tests; passes |
| `Contracts/contract-declarator-positions.cpp` | A | imported; expectations re-worded to our diagnostics ('function contract specifiers are only allowed on function declarations'); passes |
| `Contracts/contract-dependent-predicate-temporary-ast.cpp` | A | imported; failed on the round-one base (was OpenBugs, `({ })` wrapper) -- fixed in round two; passes |
| `Contracts/contract-friend-deferred-mismatch.cpp` | B | skipped: pins the ACCEPTING behaviour of a friend-contract mismatch (contradicts R14; our Clang diagnoses it, see friend-contract-mismatch.cpp) |
| `Contracts/contract-group-attr.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording; his uses `group:semantic`, our driver spells it `group=semantic`, and ours has more cases) |
| `Contracts/contract-in-declarator-group.cpp` | A | imported; expectations re-worded to our diagnostics ('function contract specifiers are only allowed on function declarations'); passes |
| `Contracts/contract-instantiate-on-use.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/contract-invalid-predicate-redecl.cpp` | A | imported verbatim; passes |
| `Contracts/contract-lambda-capture-constify.cpp` | A | imported; expectations re-worded to the round-two diagnostic ('cannot assign to variable ... considered const inside of a contract'); passes |
| `Contracts/contract-on-function-typedef.cpp` | A | imported; expectations re-worded to our diagnostics ('function contract specifiers are only allowed on function declarations'); passes |
| `Contracts/contract-on-non-function.cpp` | A | imported; expectations re-worded to our diagnostics (same as above; -fcontracts-p3850 dropped; no secondary 'pre ignored' diagnostic for `V V() pre(1)`); passes |
| `Contracts/contract-postcondition-constexpr.cpp` | A | imported; round two reports the failed postcondition as a note under the constant-expression error ('contract violation with the enforce evaluation semantic ...'); passes |
| `Contracts/contract-redecl-mismatch-diagnostics.cpp` | A | imported verbatim; passes |
| `Contracts/contract-reentrancy-basic.cpp` | A | imported verbatim; passes |
| `Contracts/contract-reentrancy-p3097.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/contract-reentrancy-p3097-p3098.cpp` | B | skipped: P3097 + P3098 |
| `Contracts/contract-reentrancy-p3098.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/contract-reentrancy-p3850.cpp` | B | skipped: P3850 umbrella (labels/messages/requires) re-entrancy |
| `Contracts/contract-sequence.cpp` | A | imported verbatim; passes |
| `Contracts/contracts-ftm-below-cxx26.cpp` | B | skipped: P3850 feature-test macros below C++26 (libc++) |
| `Contracts/contracts-inactive.cpp` | A | imported verbatim; passes |
| `Contracts/contract-specifier-ordering.cpp` | A | adapted: virtual cases also expect the R14 'virtual function cannot have contracts' error; the GNU-attribute route and requires-after-contract only get generic diagnostics here (FIXME noted); passes |
| `Contracts/contracts-std-versions.cpp` | B | skipped: tests his driver enabling -fcontracts by default for -std=c++26/c++2d (ours does not) |
| `Contracts/coroutine-compat.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording; ours additionally covers the coroutine postcondition rule) |
| `Contracts/coroutine-postcondition-param.cpp` | A | imported; expectations re-worded to our diagnostics ('postcondition of a coroutine cannot odr-use non-reference parameter'; anchored on the post line); passes |
| `Contracts/C/param-declarator-contract.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/post-no-result.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/post-observe.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/pre-enforce.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/pre-ignore.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/pre-observe.c` | B | skipped: C contracts (P4299) |
| `Contracts/C/stringize.c` | B | skipped: C contracts (P4299) |
| `Contracts/debug-info.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/deduce-return-type.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording; his dropped -verify) |
| `Contracts/default-handler-noexcept.cpp` | B | skipped: needs <contracts> + default handler noexcept (libc++ runtime API) |
| `Contracts/friendship.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/ftm-value.cpp` | A | imported; -std=c++2d RUN line dropped (not accepted by this Clang); passes |
| `Contracts/initialization.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/inline-methods.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/lambda-capture-constify-generic-function.cpp` | A | imported; expectations re-worded to our diagnostics ('implicit capture of local entity ... not allowed when used exclusively in contract assertions', no 'add to capture list' note); passes |
| `Contracts/lambda-capture-in-contract-error.cpp` | A | imported verbatim; passes |
| `Contracts/lambda-capture-scope-diagnostics.cpp` | C | ours kept: rename of our lambda2.cpp with only wording changes |
| `Contracts/lambda-contract-diagnostics.cpp` | C | ours kept: rename of our lambda.cpp with only wording changes |
| `Contracts/lambda-in-generic-lambda-smoke.cpp` | A | imported; expectations re-worded to our diagnostics ('implicit capture of local entity ... not allowed when used exclusively in contract assertions', no 'add to capture list' note); passes |
| `Contracts/lambda-templ.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording; his version replaces (void)+[] with an unused-result warning) |
| `Contracts/lit.local.cfg` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/matrix-exceptspec.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-exceptspec-errors.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-exceptspec-p3097.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/matrix-exceptspec-p3098.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/matrix-exceptspec-p3400.cpp` | B | skipped: P3400 labels |
| `Contracts/matrix-readers-caller-codegen.cpp` | B | skipped: P3595 caller-side configuration file |
| `Contracts/matrix-readers-caller-codegen.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/matrix-readers-caller.cpp` | B | skipped: P3595 caller-side configuration file |
| `Contracts/matrix-readers-caller.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/matrix-readers-codegen.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-readers.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-readers-p3097-codegen.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/matrix-readers-p3097.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/matrix-sfinae-codegen-errors.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-sfinae.cpp` | A | imported verbatim; passes |
| `Contracts/matrix-sfinae-errors.cpp` | A | imported; the declared-only cell moved to OpenBugs/contracts-needed-on-odr-use.cpp; rest passes |
| `Contracts/matrix-sfinae-p4283.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/matrix-sfinae-p4283-errors.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/matrix-sfinae-p4283-run.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/matrix-throwpaths-p3850-run.cpp` | B | skipped: P3850 runtime throw paths (libc++ handler API) |
| `Contracts/nested-class-contracts.cpp` | A | imported verbatim; passes |
| `Contracts/nested-lambda-contract-capture-depth.cpp` | A | imported; expectations re-worded (implicit-capture wording, then round-two constification wording); passes |
| `Contracts/OpenBugs/address-before-ctor-member.cpp` | D | kept in OpenBugs with XFAIL (core-language constexpr bug, no contracts) |
| `Contracts/OpenBugs/address-before-ctor-vbase.cpp` | D | kept in OpenBugs with XFAIL: with C++26 constexpr virtual bases, Clang accepts it like GCC (core-language constexpr bug, no contracts) |
| `Contracts/OpenBugs/deferred-friend-contract-mismatch.cpp` | D | our Clang already diagnoses it; moved to Contracts/friend-contract-mismatch.cpp (XFAIL removed, notes added); passes |
| `Contracts/OpenBugs/postcondition-undeduced-result.cpp` | D | his tree passes it; moved to Contracts/postcondition-undeduced-result.cpp with our wording; round two also emits the void-result recovery error for shape 2 (expected now); passes |
| `Contracts/OpenBugs/retval-throwing-cleanup.cpp` | D | kept in OpenBugs with XFAIL (core-language codegen bug, no contracts; %libcxx_flags dropped) |
| `Contracts/OpenBugs/this-in-xobj-noexcept-specifier.cpp` | D | kept in OpenBugs with XFAIL (core-language P0847 bug, no contracts) |
| `Contracts/OpenBugs/this-in-xobj-trailing-return-type.cpp` | D | kept in OpenBugs with XFAIL (core-language P0847 bug, no contracts) |
| `Contracts/over.call.func.p3.1.cpp` | A | imported; was OpenBugs (member access in ctor-pre / dtor-post accepted) -- fixed in round three, now Contracts/over.call.func.p3.1.cpp with our wording and more cases; passes |
| `Contracts/p2900-constexpr-potential-postcondition.cpp` | A | imported, fails here -> OpenBugs/p2900-constexpr-potential-postcondition.cpp (XFAIL): -Winvalid-constexpr skips postconditions (quality, not conformance) |
| `Contracts/p3097-basic-sema.cpp` | B | skipped: P3097 virtual-function contracts |
| `Contracts/p3097-config-semantic.cpp` | B | skipped: P3097 virtual-function contracts |
| `Contracts/p3097-config-semantic.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3097-ftm.cpp` | B | skipped: P3097 virtual-function contracts |
| `Contracts/p3097-no-flag.cpp` | B | skipped: expects his '-fcontracts-p3097' diagnostic; R14 rejection of virtual contracts is covered by our parsing-ambiguities.cpp |
| `Contracts/p3097-override-sema.cpp` | B | skipped: P3097 virtual-function contracts |
| `Contracts/p3097-virtual-constexpr.cpp` | B | skipped: P3097 virtual-function contracts |
| `Contracts/p3098-capture-constexpr.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-capture-destructible.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-capture-member-template.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-ftm.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-name-lookup.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-noflag.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-noncopyable.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-pack-basic.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-pack-errors.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-parse-errors.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-redecl.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-templates.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3098-valid-parse.cpp` | B | skipped: P3098 postcondition captures |
| `Contracts/p3099-message-basic.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-message-conflict.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-message-constexpr.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-message-ftm.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-message-no-flag.cpp` | A | adapted: renamed to no-message-syntax.cpp (R14 has no message operand; diagnostics are plain parse errors); passes |
| `Contracts/p3099-message-redecl-cexpr.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-message-redecl.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3099-static-assert-message.cpp` | B | skipped: P3099 contract messages |
| `Contracts/p3100-alignment-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-allow-assume-ftm.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-allow-assume-no-flag.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-codegen.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-codegen.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds-onepast.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-onepast.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-array-bounds-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-array-bounds-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-asan-descriptor.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-codegen.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-codegen.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-enforce-predicates.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-enforce-predicates.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-predicates.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-predicates.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-pure.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-pure-group.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-pure-group.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-pure.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-throw.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-throw.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-assume-attr-throw-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-assume-attr-throw-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-compute-assume-error.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-coro-flow-off.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-coro-flow-off.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-coro-flow-off-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-coro-flow-off-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-coro-flow-off-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-coro-flow-off-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-coro-flow-off-throw.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-coro-flow-off-throw.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero-ignore.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-by-zero-throw-cleanup.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-throw-fntryblock.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-throw-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-by-zero-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-eval.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-overflow.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-overflow-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-overflow-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-overflow-independent.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-div-overflow-independent.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-overflow.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-div-types.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-enum.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-default.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-enforce-aggregate.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-fntryblock.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-fntryblock-handler.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-fntryblock.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-fntryblock-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-fntryblock-noexcept-terminate.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-fntryblock-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-ignore.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-ignore.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-observe.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-throw-cleanup.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-throw-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-throw-noexcept-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-throw-noexcept-observe.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-throw-no-return-dtor.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-throw-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-flow-off-throw-trycatch.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-zero-return.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-flow-off-zero-return-observe.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-fpint-eval.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint-ignore.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-fpint.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-fpint-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-fpint-throw.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-fpint-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-fpint-types.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-ftm.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-function-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-implicit-constexpr.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-implicit-ftm.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-implicit-no-flag.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-codegen.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-codegen.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-invalid-value.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-invalid-value.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-invalid-value-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-invalid-value-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-invalid-value-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-invalid-value-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-mod-by-zero-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-mod-by-zero-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-mod-by-zero-throw.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-msan-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-noexcept-nested-ce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-noexcept-operator.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-noexcept-operator.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-no-flag.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-addr-of.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-assume.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-noexcept-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-noexcept-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-deref-noexcept-observe.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-noexcept-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-deref-observe.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-deref-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-deref-resolution.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-resolution.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-deref-throw-cleanup.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-throw-fntryblock.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-throw-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-deref-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-null-ref-and-call.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-null-ref-and-call.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow-codegen.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-codegen.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow-noexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-noexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-overflow-throw-nonnoexcept.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-overflow-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-p3850-implies.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-p3850-implies-routing.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-sanitize-semantic-no-p3100.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob-enforce.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob-enforce.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-shift-oob-eval.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob-ignore.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-shift-oob.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-shift-oob-quick.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob-quick.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-shift-oob-throw.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-shift-oob-throw-nonnoexcept.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3100-shift-oob-types.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-tsan-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3100-vptr-sanitize-semantic.cpp` | B | skipped: P3100 erroneous-behaviour/sanitizer routing |
| `Contracts/p3290-assert-ftm-assert-h.cpp` | B | skipped: P3290 manual violation API |
| `Contracts/p3290-assert-ftm.cpp` | B | skipped: P3290 manual violation API |
| `Contracts/p3290-assert-ftm-version.cpp` | B | skipped: P3290 manual violation API |
| `Contracts/p3290-ftm.cpp` | B | skipped: P3290 manual violation API |
| `Contracts/p3290-no-flag.cpp` | B | skipped: P3290 manual violation API |
| `Contracts/p3400-bypass-rethrowing-local-handler-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-bypass-rethrowing-local-handler-disable-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-bypass-rethrowing-local-handler-negative-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-bypass-rethrowing-local-handler-noexcept-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-bypass-rethrowing-local-handler-semantics-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-combine-basic.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-combine-lookup.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-adl.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-basic.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-contexts.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-expr.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-function.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-header.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-control-scope.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-allowed.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-allowed-error.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-allowed-noexcept.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-combined.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-comment.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-empty-combine.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-group.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-local-handler.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-message.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-near-miss.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-non-constexpr.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-permutations.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-facet-review.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-group-config.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-bad-type.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-basic.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-ftm.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-late-parsed.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-no-flag.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-operators.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-prvalue.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-prvalue-template.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-redecl.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-rshift.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-stateful-allowed.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-stateful-facet.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-label-trailing-return.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3400-local-handler-exception-path-codegen.cpp` | B | skipped: P3400 labels / local handlers |
| `Contracts/p3595-caller-object-parse-bad.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-caller-object-parse.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-caller-object-parse-good.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-caller-unimplemented-warn-1.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-caller-unimplemented-warn-2.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-caller-unimplemented-warn-3.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-caller-unimplemented-warn.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-combined-match.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-combined-match.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-errors.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-file.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-file.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-kind.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-kind.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-line-range.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-line-range.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-location.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-location-error.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-location-error.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-location.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-multi-default.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-multi-file.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-multi-pre.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-namespace.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-namespace.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-order.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-order-override.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-template.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-template.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-config-wrong-type.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-config-wrong-type.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-cname.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-cname.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-constexpr.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-constexpr.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-constexpr-skip.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-constexpr-skip.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-cxxname.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-cxxname.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-err-empty.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-err-linkage.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-err-noname.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-errors.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-err-pw.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-mangle-nested.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-mangle-nested.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-noweak-ir.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-noweak-ir.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-parse.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-parse.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-samesym-ir.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-samesym-ir.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3595-dynamic-template-skip.cpp` | B | skipped: P3595 contract configuration |
| `Contracts/p3595-dynamic-template-skip.json` | B | skipped: configuration file for his -fcontract-configuration-file (P3595) |
| `Contracts/p3850-implies.cpp` | B | skipped: P3850 umbrella |
| `Contracts/p4283-basic.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-errors.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-ftm.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-ftm-undefined.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-malformed.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-modules.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-no-flag.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-redecl.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-requires-grammar.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-requires-member.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4283-requires-with-attribute.cpp` | B | skipped: P4283 requires-clauses on contracts |
| `Contracts/p4298-constexpr-terminating.cpp` | B | skipped: P4298 noexcept_* semantics |
| `Contracts/p4298-flag.cpp` | B | skipped: P4298 noexcept_* semantics |
| `Contracts/p4298-ftm-undefined.cpp` | B | skipped: P4298 noexcept_* semantics |
| `Contracts/p4301-report-ftm.cpp` | B | skipped: P4301 contract_violation::report |
| `Contracts/p4301-report-ftm-undefined.cpp` | B | skipped: P4301 contract_violation::report |
| `Contracts/p4301-report-noexcept.cpp` | B | skipped: P4301 contract_violation::report |
| `Contracts/p4301-report-no-flag.cpp` | B | skipped: P4301 contract_violation::report |
| `Contracts/parse-contract-basics.cpp` | C | ours kept: rename of our contracts.cpp with only wording changes |
| `Contracts/parse-contract-grammar.cpp` | C | ours kept: rename of our parsing-ambiguities.cpp with only wording changes plus P3097 relaxations (virtual/deleted/defaulted errors removed) that contradict R14 |
| `Contracts/postcondition-param-and-capture-diagnostics.cpp` | C | ours kept: rename of our postcondition-params-and-lambda-captures.cpp with only wording changes |
| `Contracts/postcondition-param-lambda.cpp` | A | imported; failed on the round-one base (was OpenBugs, non-generic lambda unchecked) -- fixed in round two; passes |
| `Contracts/postcondition-redecl-no-diagnostics.cpp` | A | imported verbatim; passes |
| `Contracts/pure-virtual-dependent-predicate-codegen.cpp` | B | skipped: P3097 virtual-function contracts (all cases virtual) |
| `Contracts/result-name-misplaced.cpp` | A | imported; expectations re-worded to our diagnostics ('result name cannot be used with a void return type'; `contract_assert(r: ...)` is a plain parse error here); passes |
| `Contracts/result-name-shadows-template-param.cpp` | A | imported; was OpenBugs (result name redeclaring a template parameter accepted) -- fixed in round three, now Contracts/result-name-shadows-template-param.cpp; passes |
| `Contracts/scope-rules.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/basic.contract.eval.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/Sema/contract-predicate-constify-in-lambda.cpp` | A | imported (captured-*this member case was an OpenBug on the round-one base, fixed in round two); by-ref-capture wording updated to round-two diagnostics; passes |
| `Contracts/Sema/contract-predicate-constify-storage.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/contract-unexpanded-pack.cpp` | A | imported; crashed on the round-one base (was OpenBugs) -- fixed in round two, now Sema/contract-unexpanded-pack.cpp; passes |
| `Contracts/Sema/decl.contract.func.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/Sema/decl.contract.res.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/Sema/deducing-this-no-this-in-predicate.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/deducing-this-postcondition-param.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/deducing-this-postcondition-param-dependent.cpp` | A | imported, fails here -> OpenBugs/deducing-this-postcondition-param-dependent.cpp (XFAIL): contracts of declared-only templates not instantiated on odr-use |
| `Contracts/Sema/expr.const.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/Sema/expr.prim.closure.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/Sema/postcondition-discarded-operand-dependent.cpp` | A | imported; the discarded-comma-operand exempt case moved to Sema/pr126897.cpp; passes |
| `Contracts/Sema/postcondition-lambda-param.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/postcondition-pack-const.cpp` | A | imported; was OpenBugs (pack-indexing odr-use not checked) -- fixed in round three, now Sema/postcondition-pack-const.cpp; passes |
| `Contracts/Sema/postcondition-pack-empty.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/postcondition-redecl-dependent-param.cpp` | A | imported, fails here -> OpenBugs/postcondition-redecl-dependent-param.cpp (XFAIL): [dcl.contract.func]/7 'all declarations' not checked for dependent parameter types |
| `Contracts/Sema/postcondition-unevaluated-operand.cpp` | A | imported verbatim; passes |
| `Contracts/Sema/pr126897.cpp` | A | imported; was OpenBugs (discarded comma operand treated as odr-use) -- fixed in round three, now Sema/pr126897.cpp with redeclaration and potential-result cases added; passes |
| `Contracts/sfinae-test.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Contracts/template-contract-diagnostics.cpp` | C | ours kept: rename of our templates.cpp with only wording changes |
| `Contracts/this-in-out-of-line-member-contract.cpp` | A | imported verbatim; passes |
| `Contracts/unused-param-in-contract.cpp` | A | imported verbatim; passes |
| `Contracts/variadic-contracts.cpp` | A | imported verbatim; passes |
| `Contracts/virtual-contracts-require-p3097.cpp` | B | skipped: expects his '-fcontracts-p3097' diagnostic; R14 rejection of virtual contracts is covered by our parsing-ambiguities.cpp |
| `Modules/contracts.cppm` | C | ours kept: his file is the unrelated upstream lambdas-in-modules test; our contract-specific one stays |
| `Parser/contract-inline-methods.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `Parser/cxx-contracts.cpp` | C | ours kept (ours is the newer/stricter version; his differs only in RUN flags/wording) |
| `CXX/drs/cwg3088.cpp` | B | not a contracts test (upstream CWG test; only macros named pre/post/contract_assert) |
