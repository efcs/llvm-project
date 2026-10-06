# Contracts: open questions

Temporary tracking document for the C++26 contracts (P2900R14) implementation.
Each entry records what the implementation does today, what the alternative is,
and who needs to decide. Remove entries as they are settled.

## 1. Wording questions (need a decision)

### 1.1 By-copy captures of the lambda that owns the contract
[expr.prim.id.unqual]p4 ("names the closure member") precedes p7 ("const inside a
contract predicate") in the "Otherwise" chain. Read literally, a variable captured
by copy by the lambda that *owns* the assertion names a modifiable closure member
in a `mutable` lambda:

```c++
[=]() mutable pre((x = 1, true)) {}                     // own pre
[=]() mutable { contract_assert((x = 1, true)); }        // body assertion
```

- Today: both rejected (constified). Chuanqi Xu's implementation and the
  pre-round-two branch also reject both.
- GCC (contracts-p3850): rejects the body case, accepts the own-`pre` case.
- The standard's p8 examples (`[=,&i,*this] mutable { ++p; ++r; ++z; }`,
  `post(r: [=] mutable { ++r; })`) are all handled correctly either way.
- Proposal: keep constifying; raise the wording with SG21/CWG.

### 1.2 Result name that redeclares a template parameter
`template <int r> int g() post(r : r > 0);` is accepted silently
(SemaContract.cpp "pretend we didn't see it"). GCC errors ([temp.local]p6: a
template parameter shall not be redeclared within its scope).
- Proposal: diagnose as an error.

### 1.3 Result name vs. init-capture of the same name
`[r = 7]() post(r : r > 0) { ... }` is accepted ([basic.scope.contract]p2.2 would
make the result name conflict with the init-capture). There is a FIXME in the code.
- Proposal: diagnose.

### 1.4 Members named in a constructor precondition / destructor postcondition
`S() pre(k)` and `~S() post(f())` are accepted. [expr.prim.id.general]/2 and the
standard's own example (`C() pre(b) // error`) make them ill-formed. Chuanqi Xu's
implementation rejects *any* use of `this` there, which is over-strict
(`this != nullptr`, `sizeof(this->m)` are fine).
- Proposal: diagnose implicit/explicit member access of the object under
  construction/destruction only. (OpenBugs/over.call.func.p3.1.cpp)

### 1.5 Destructor postconditions that name members
`constexpr ~M() post(m == 0) { m = 0; }` used in constant evaluation: the AST
evaluator reports a violation ("read of object outside its lifetime" — the
postcondition runs after member destruction), the bytecode interpreter accepts,
GCC rejects member use in a destructor postcondition outright.
- Question: what is the evaluation point of a destructor postcondition relative to
  member destruction, and is naming a member ill-formed?

### 1.6 Constant destruction of static objects with destructor contracts
The constant-destruction *test* ignores contracts (permitted by [expr.const] fn.
63), but the implementation then elides the destructor, so an `enforce` contract
in `~D` of a static object is never evaluated at all ([basic.stc.static]p2 forbids
eliminating a destructor with side effects).
- Proposal: recheck with contracts after a successful trial (like constant
  initialization), and force dynamic destructor registration when a checked
  assertion was skipped.

### 1.7 Allocation escaping an observed predicate
`pre((*pp = new int(5), true))` under `observe`: the evaluator discards the
allocation as "leaked" and a later read in the enclosing evaluation fails with
"read of heap allocated object that has been deleted". Defensible per
[expr.const]p2.20 with the predicate as the expression E (GCC agrees); the note is
misleading and the outer expression could arguably still be constant.

### 1.8 Postcondition comment text
`contract_violation::comment()` for `post(r : r > x)` is `"r : r > x"` (result
name introducer included). GCC and Chuanqi Xu's implementation report `"r > x"`.
The text is implementation-defined. Keep or align with GCC?

### 1.9 Termination under `enforce`: `std::abort()` vs `std::terminate()`
Round one called `std::terminate()` after the handler returned under `enforce`
(the user's terminate handler ran); the P3850 ABI entry points call
`std::abort()` (SIGABRT, no terminate handler), and `quick_enforce` traps
(SIGILL). Both are allowed: contract termination is implementation-defined in
[basic.contract.eval]. The ABI spec (§2.1) mandates `abort()`. Keep `abort()`?
Note that P2900's "the implicit handler remains active" wording only has meaning
for the terminate path.

### 1.10 Predicate text: as spelled vs. as expanded
`comment()` and the diagnostics use the predicate as spelled in the source
(`"GT(x, 0)"` for a macro-built predicate); GCC uses the expanded form
(`"((a)>(b))"`). Both are implementation-defined. Keep "as spelled"? (Separately,
preprocessor directive lines inside a multi-line predicate currently leak into
the text — that is a bug, see round three.)

### 1.11 Static analyzer under `observe`
`int f(int *p) pre(p != nullptr) { return *p; }` reports a null dereference under
`observe` only, because an observed predicate is modelled as a branch that may be
false. Arguably right (the function does continue after a violation), but it will
be noisy. Model observed predicates as assumptions too, or keep the precise model?

## 2. Itanium contracts ABI (P3850) — spec issues to raise with Joshua Berne

Found while implementing the spec (`specification-proposal.rst`, branch
`contracts-p3850`). Our choice in brackets.

1. §4.2 says 5 kinds × 2 × 2 × 2 = 40 specialized entry points, but the "Total:
   34 (2 + 32)" line and requirements.rst still say 4 kinds / 32. [40 + 2 exported]
2. §5.5 offset table (`4*sizeof(void*)+4`, `5*sizeof(void*)`) is only right on
   64-bit; on 32-bit the comment field is at `2*ptr + sizeof(__cxa_source_location)`.
   §6.3's formula is right. [follow §6.3]
3. §4.5/§6.1 step 3 check the weak C++ symbol `handle_contract_violation`; §8.4
   checks the C-linkage `__handle_contract_violation` compiler alias; §8.3 says
   Tier 2 provides a weak *default* `handle_contract_violation`, contradicting
   §8.4's "absent a user definition the symbol is undefined". GCC/libstdc++ defines
   weak defaults of both; Joshua's libc++ defines neither. [§8.4 + weak C++ default]
4. §8.4 says the handler receives "the data-block chain head"; both
   implementations and §6.4 pass a pointer to an object holding the chain pointer.
   [follow the implementations]
5. The example header lags the text: `__cxa_find_field` has C++ linkage, no
   `CXA_AK_IMPLICIT`, `CXA_FIELD_REPORT`, or the `implicit` entry points.
6. Both reference implementations add `CXA_ES_NOEXCEPT_OBSERVE/ENFORCE` (0x06/0x07,
   D4298) and `__cxa_contract_violation_{kind}_noexcept_{observe,enforce}_*` entry
   points that are not in the spec. [omitted; GCC objects using them fail to link]
7. §8.3/§8.7 require identical Tier-2 symbol names across runtimes, but the
   underlying types of the `std::contracts` enums (part of the return ABI of
   `kind()` etc.) are unspecified and differ: libstdc++ `uint16_t` ×3, Joshua's
   libc++ `uchar/uint16/uchar`, ours `unsigned char` ×3. [unsigned char; decide]
8. Field 0x0A (`void*` exception) semantics are unspecified. [the thrown-object
   pointer as `exception_ptr` holds it]
9. Universal entry point with `quick_enforce` (or unspecified) semantic in the
   chain is unspecified. [abort for quick_enforce, return otherwise; GCC/Joshua
   abort only for enforce]
10. §8.4's alias requires compiler alias support; Mach-O has none (GCC skips it).
    [emit a `llvm.used` thunk]
11. The reference compilers select `_noexcept` entry points only for the D4298
    semantics, never for noexcept contexts, contrary to the rationale.
    [select `_noexcept` from the EH context]
12. Message field: clang emits `""` when there is no message; GCC emits a null
    pointer and libstdc++'s `message()` distinguishes "absent" from "empty".
    [resolved: clang emits null for an absent message and `""` for an empty one
    (`[[clang::contract_message("")]]`); the default handler treats both as
    absent, and `comment()` is never null]
13. P3400 local-handler blocks (`CXA_FIELD_LOCAL_HANDLER`/`CXA_FIELD_LABEL_PTR`)
    emitted by GCC-compiled TUs are ignored by our runtime (spec §6.1 step 2).
    Support the extension in libc++abi, or document that it is not supported?

## 3. Implementation decisions pending

- **Ownership of contracts.** Today each redeclaration carries its own
  `ContractSpecifierDecl` and a definition that omits the contracts gets a
  TreeTransform copy (wrong source locations, duplicate diagnostics, lambdas
  cloned into new closure types, several OpenBugs). Planned: the first declaration
  owns the contracts, parameters are bound by function-scope index, CodeGen aliases
  the first declaration's parameters and the result name. Prerequisite for
  caller-side checks and P3097.
- **Template contracts are instantiated only with the definition.**
  [dcl.contract.func]/9 requires them when the function is odr-used; a
  declared-only specialization with an ill-formed predicate is accepted
  (OpenBugs/contracts-needed-on-odr-use.cpp and two siblings). Planned: on-demand
  instantiation like exception specifications.
- **Evaluation semantic lives in `LangOptions`.** Not a `LANGOPT`, so not part of
  the PCH/module compatibility check; a PCH built with `ignore` bakes constant
  evaluation results an `enforce` consumer reuses. Planned: runtime semantic and
  group map move to CodeGenOptions behind one hook; the constant-evaluation
  semantic becomes a real `ENUM_LANGOPT` with a deliberate compatibility kind.
- **`-fcontracts` in C++26 mode.** Still required; `contract_assert` is not reserved
  without it. Upstream reviewers (llvm/llvm-project#221139) want a cc1-only flag
  during bring-up and the keyword reserved in C++26.
- **Vendor extensions to keep, rename, or split out before upstreaming:**
  `__pre`/`__post`/`__contract_assert` (`__pre`/`__post` are Win32 SAL macros that
  libc++'s reserved-name tests forbid), `[[clang::contract_group]]` +
  `-fcontract-group-evaluation-semantic=` (string groups compete with P3400 label
  objects), `[[clang::contract_message]]`, `_LIBCPP_HARDENING_USE_CONTRACTS`.
- **Lambdas in predicates across translation units are never compared.** The AST
  reader merges corresponding closure types before the contract comparison runs,
  so two TUs declaring `int f(int x) pre([](int v){ return v > 0; }(x))` with
  different lambda bodies are not diagnosed by `-Wmismatched-contract-specifiers`
  (the closure-type ODR check that would catch it is suppressed when contract
  entities were renamed). No diagnostic required (NDR), but worth knowing; a fix
  needs the comparison to run on the unmerged closure definitions.
- **Runtime hosting when libc++ is not built on libc++abi** (Windows/vcruntime,
  libcxxrt, libsupc++): the contracts runtime is currently compiled out entirely.
  Host the entry points in libc++ as a fallback, or declare the configuration
  unsupported with a diagnostic?
- **Compile-time cost of trial-with-ignore.** Constant-initialized non-constexpr
  globals with checked contracts are evaluated twice (≈2× on a synthetic case).
  Acceptable, or cache the trial evaluation?
- **Diagnostic-note collection for failed predicates** costs 10–14% on a synthetic
  Sema-folding workload whose predicates fail, even when nobody reads the notes.
  Collect only when the enclosing evaluation has a diagnostic sink?
