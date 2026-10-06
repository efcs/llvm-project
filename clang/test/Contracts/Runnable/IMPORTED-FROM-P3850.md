# Tests imported from Joshua Berne's `contracts-p3850` branch

*Temporary working document for Eric: a triage table for the runnable and
libc++ contract tests taken from Josh's branch. Delete once the import has
been reviewed.*

Source: `notadragon/llvm-project` branch `contracts-p3850`
(`clang/test/Contracts/Runnable/**`, 315 files = 279 `.cpp` + 31 `.json` +
5 headers; `libcxx/test/std/contracts/*`, 2 files). That branch implements
C++29 papers (P3097 virtual-function contracts, P3098 postcondition captures,
P3099 contract messages, P3100 UB-as-contract-violation / sanitizer routing,
P3290 manual violation API and `assert`, P3400 labels and local handlers,
P3595 dynamic semantic configuration, P4283 constrained contracts, P4298
`noexcept_*` semantics, P4299/P4301 C API and `report()`) on top of its own
P3850 runtime ABI, so most of its tests are not C++26 tests.

Categories:

* **A** – C++26 / P2900R14 behaviour: imported.
* **B** – C++29 extension (or depends on his runtime API): skipped.
* **C** – duplicate / superset of a test we already have: his taken, merged,
  or ours kept.
* **D** – depends on the P3850 ABI entry points or his `contracts.h` /
  `stdcontracts.h` headers: would go to `abi-p3850/` (none qualified, see
  below).
* **E** – his known failures: none exist in these directories.

## Infrastructure

* `Runnable/lit.local.cfg` (new): defines `%libcxx_flags`
  (`-stdlib=libc++ -L<libdir> -Wl,-rpath,<libdir>`) and the lit feature
  `contracts-libcxx`, available only when the runtimes build produced libc++.
  Every imported runnable test says `REQUIRES: contracts-libcxx` and uses
  `<contracts>` and the standard `::handle_contract_violation` replacement,
  never an entry-point name, so it works with both the current
  `__handle_contract_violation_v3` runtime and the P3850 one.
* His support headers (`contracts.h`, `contracts-runtime.h`, `my_assert.h`,
  `event_sequence.h`, `source-location.h`) only differ from ours by the
  C++29 enumerators / v4 ABI struct; ours are kept unchanged.
* RUN-line adaptations: `-fcontracts-p3850` dropped (it only selects his
  ABI), `-fcontract-group-evaluation-semantic=g:s` respelled as `g=s`.

## Category A: imported (verbatim or near-verbatim, author Joshua Berne)

| File | What it checks | Outcome |
|---|---|---|
| allowed-mask-quick-enforce.cpp | quick_enforce traps on a violated pre (`not --crash`) | pass |
| contract-assert-handler-throw-catchable.cpp | handler exception from `contract_assert` is caught by an enclosing try in the same function | pass |
| contract-declarator-positions.cpp | contracts after complicated declarators / trailing return types bind to the function and see its parameters | pass |
| contract-dependent-predicate-temporary.cpp | dependent predicate holding a non-trivially-destructible temporary, in-class decl + out-of-line definition | **XFAIL** (clang crashes, see below) |
| contract-result-binding-identity.cpp | one result binding denotes one object; `&r == ptr` for a non-trivially-copyable class | pass |
| contract-result-binding-mutation-class.cpp | mutation through the result binding (class results, coerced / sret) is observable by the caller | pass |
| contract-result-binding-mutation.cpp | mutation through the result binding (scalar results) is observable; no temporary for non-trivial class | pass |
| contract-retval-destroyed-on-unwind.cpp | handler throwing from a postcondition destroys the already-initialized return object exactly once | pass |
| coroutine-contract-parameters.cpp | contracts on a coroutine name the ramp's parameters, not the frame copies | pass |
| evaluation-order.cpp | pre (in order), body, post | pass |
| function-try-block-no-catch.cpp | a function-try-block cannot catch a handler exception from a precondition | pass |
| lambda-capture-contract-in-template.cpp | lambda contract predicates naming captures, inside templates | pass |
| lambda-capture-in-contract.cpp | lambdas written inside predicates may capture parameters ([expr.prim.lambda.capture]/3.3) | pass |
| lambda-contract-in-template.cpp | lambda contracts are substituted when the lambda is instantiated as part of a template | pass |
| lambda-in-postcondition-result-name.cpp | lambda in a postcondition predicate that has a result name | pass |
| library-compiler-interface.cpp | (replaces ours, category C) semantic / location / group selection through the public `contract_violation` API | pass |
| p3595-enforce-kind.cpp | enforce + `-fno-exceptions` reports `kind() == pre`, not `assert` | pass |
| p4298-noexcept-fn-inline.cpp | handler throws inside a `noexcept` function under enforce: terminate | pass |
| postcondition-result-name-mangling.cpp | result name does not disturb mangling of overloads | pass |
| postcondition-throw-escapes-try.cpp | handler exception from a function contract escapes every try inside the body; `contract_assert` inside a try is caught | pass |
| postcondition-throw-noexcept-terminate.cpp | handler exception from a postcondition on a `noexcept` function terminates | pass |
| result-name-reference-return.cpp | result binding on a reference-returning function names the referred-to object | pass |
| smf-handler-throws-terminate.cpp | handler throws from a `noexcept` constructor's precondition: terminate, ctor function-try-block does not catch | pass |
| violation-comment-kind.cpp | `kind()` for pre/post/assert and `comment()` text | pass |
| violation-handler-throws-noexcept.cpp | handler throws under observe in a `noexcept` function: terminate | pass |
| violation-handler-throws-observe.cpp | handler throws under observe: propagates to the caller | pass |
| violation-template-function-name.cpp | `location().function_name()` names the template instantiation | pass |
| void-postcondition-explicit-return-test.cpp | postcondition on a void function with explicit `return` | pass |
| void-postcondition-test.cpp | postcondition on a void function falling off the end | pass |

## Category A: imported with C++29 parts removed (author Eric Fiselier, based on his tests)

| File | Removed | Outcome |
|---|---|---|
| contract-classtemplate-outofline.cpp | the P3097 virtual-member case | pass |
| contract-reentrancy.cpp | the P3097 (virtual) and P3098 (capture) re-entrancy cases; a nested free-function-template case added | pass |
| deducing-this-runtime.cpp | the P3098 `post[old = self.x]` case | pass |
| noexceptions-contracts.cpp | the P3098 capture shapes and the P3097 virtual/override cases | pass |

## Category C: already covered by our tests

| His file | Ours | Decision |
|---|---|---|
| breathing-test.cpp | same file | ours kept (his diff: `-std=c++26` + `%libcxx_flags`) |
| contract-exceptions.cpp | same file | ours kept (his diff: `-std=c++26`) |
| contract-result-name.cpp | same file | ours kept (his diff: `-std=c++26`) |
| decl.contracts.res.cpp | same file | ours kept (his diff: `<contracts>` instead of the stub header) |
| lambda-test.cpp | same file | ours kept (same content modulo helper names; both already count through function calls) |
| library-compiler-interface.cpp | same file | **his taken**: ours tested the v3 ABI struct directly, his tests the same behaviour through the public API |
| libcxx/test/std/contracts/violation_handler.pass.cpp | support.contract/contract_violation.pass.cpp | ours is a superset (also checks location, function name, evaluation_exception); not imported |
| libcxx/test/std/contracts/enums.compile.pass.cpp | support.contract/enums.compile.pass.cpp | ours covers the R14 enumerators; his additionally pins C++29 enumerators (`manual`, `cassert`, `unspecified`), `evaluation_semantic_set` and libstdc++-compatible underlying types (his ABI); not imported |

## Category B: C++29 extensions, skipped

All of these need a `-fcontracts-pNNNN` flag, a semantic we do not have
(`assume`, `noexcept_enforce`, `noexcept_observe`), `-fcontract-configuration-file=`,
`-fsanitize-semantic=`, or runtime API we do not have (`message()`,
`report()`, `evaluation_semantic_set`, `handle_observed_contract_violation`,
`stdc_contract_violation_*`).

| Family | Files | Paper |
|---|---|---|
| p3097-*.cpp | 31 | P3097 contracts on virtual functions (ill-formed in R14: [dcl.contract.func] "a virtual function ... shall not have a function-contract-specifier-seq") |
| p3098-*.cpp | 22 | P3098 postcondition captures `post[x = ...]` |
| p3099-*.cpp | 11 | P3099 contract messages / `contract_violation::message()` (incl. p3099-message-attribute.cpp, which only needs `[[clang::contract_message]]` but reads `message()`) |
| p3100-*.cpp, p3100-*.json | 98 + 18 | P3100 UB / sanitizer checks routed as contract violations, `assume` semantic |
| p3290-*.cpp | 13 | P3290 manual violation API, `assert` routed to the handler |
| p3400-*.cpp | 31 | P3400 labels, local handlers, facets |
| p3595-dynamic-*.cpp, *.json | 14 + 12 | P3595 dynamic semantic selection via configuration file |
| p4283-*.cpp | 5 | P4283 constrained / discarded contracts |
| p4298-*.cpp (except -noexcept-fn-inline), p4298-json-config.json | 12 + 1 | P4298 `noexcept_enforce` / `noexcept_observe` |
| p4299-c-is-terminating-noexcept.cpp | 1 | C API `stdc_contract_violation_is_terminating` |
| p4301-report-*.cpp | 2 | P4301 `contract_violation::report()` (p4301-report-noexcept-fallback.cpp also hand-builds a P3850 ABI data block) |
| contract-reentrancy.cpp (original) | – | imported trimmed, see above |
| p3098-capture-member-late.cpp | 1 | P3098 capture in a class template member |

## Category D: ABI-dependent

Nothing qualified: the only files that reference `__cxa_contract_violation*`,
`<__contracts/abi.h>` or his C API are also C++29 tests (p3100-*-throw-noexcept-*,
p4301-report-noexcept-fallback.cpp, p4299-c-is-terminating-noexcept.cpp),
listed under B. `abi-p3850/` was therefore not created.

## Results (borg, Release+asserts, x86_64 Linux)

`llvm-lit clang/test/Contracts/Runnable`: 38 pass, 1 expectedly failing
(the whole `clang/test/Contracts` directory: 69 pass + 1 XFAIL). The
pre-existing libc++ suites (`std/contracts`, `support.contract`,
`libcxx/assertions`, `support.limits`, `transitive_includes`, `module_std`)
are unchanged: 259 pass, 5 unsupported.

### Failures triaged

| Test | Area | Verdict | Detail |
|---|---|---|---|
| contract-dependent-predicate-temporary.cpp | sema (template instantiation) | (i) our bug, `XFAIL: *` | A function template declared with a type-dependent predicate that materializes a temporary with a non-trivial destructor, then defined out of line without the contract: rebuilding the contract for the definition diagnoses "value of type 'void' is not contextually convertible to 'bool'" (and, with a result name, "capturing result name 'R' ... not supported yet") and segfaults in `TreeTransform::TransformAttr` under `Sema::InstantiateContractSpecifier`. Contract on the definition, trivially-destructible temporary, non-dependent predicate and non-template all work. Minimal repro: `template <class T> void f(const T &V) pre(Guard(10) <= V); template <class T> void f(const T &V) {} int main() { f(20); }` with `struct Guard { Guard(long); ~Guard() {} }`. |

No expectation in the imported tests had to be corrected against P2900R14.
Two expectations worth knowing about, both satisfied by our implementation:

* violation-comment-kind.cpp expects the postcondition `comment()` to be
  `"r : r > x"`, i.e. including the result-name introducer (Josh notes this as
  a logged Clang/GCC divergence; GCC reports `"r > x"`). R14 leaves the
  comment text implementation-defined.
* contract-result-binding-mutation*.cpp expect a mutation performed through
  the result binding (`const_cast`) to be observable by the caller, including
  for scalar and register-returned class results. R14 [dcl.contract.res]
  permits a temporary for such results (Example 2), so losing the mutation
  would arguably also be conforming; the tests pin the stronger, GCC-matching
  behaviour, which Clang already has.
* contract-retval-destroyed-on-unwind.cpp deliberately runs ahead of the
  wording (cplusplus/cwg#988): a handler throwing from a postcondition
  destroys the already-initialized return object. Our codegen does that
  (round-one commit "Destroy the return object if a postcondition throws").
