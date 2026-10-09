# RUN: %{python} %s

# Verify that the ABI lists of libc++ (libcxx/lib/abi/*.abilist) agree on the
# symbols of the C++26 contracts support: the std::contracts symbols libc++
# defines, and the symbols of the contract-violation runtime of the Itanium
# C++ Contracts ABI (libcxxabi/lib/contracts.exp), which libc++ references,
# re-exports on Apple platforms, and defines where libc++abi is merged into it.
# Only one of the lists is regenerated routinely; the others are edited by
# hand and this keeps them from drifting apart.

import ast
import glob
import os
import re

if __name__ == "__main__":
    lint_dir = os.path.dirname(os.path.abspath(__file__))
    libcxx = os.path.abspath(os.path.join(lint_dir, "../../.."))
    abi_dir = os.path.join(libcxx, "lib/abi")
    exp_file = os.path.join(libcxx, "../libcxxabi/lib/contracts.exp")
    assert os.path.isdir(abi_dir)
    assert os.path.isfile(exp_file)

    # The runtime's symbols, as Darwin names them (with a leading underscore).
    with open(exp_file, "r") as f:
        runtime_symbols = {line.strip()[1:] for line in f if line.strip() and not line.startswith("#")}
    assert "__cxa_find_field" in runtime_symbols
    assert "__contract_invoke_default_handler" in runtime_symbols
    assert "__handle_contract_violation" in runtime_symbols
    entry_points = {s for s in runtime_symbols if s.startswith("__cxa_contract_violation")}
    assert len(entry_points) == 42, entry_points

    def is_contracts_symbol(name):
        return "contract" in name or name == "__cxa_find_field"

    def load(path):
        with open(path, "r") as f:
            symbols = [ast.literal_eval(line) for line in f if line.strip()]
        if "apple-darwin" in os.path.basename(path):
            for sym in symbols:
                assert sym["name"].startswith("_"), sym
                sym["name"] = sym["name"][1:]
        return [sym for sym in symbols if is_contracts_symbol(sym["name"])]

    def defined_cxx(symbols):
        return {s["name"] for s in symbols if s["is_defined"] and s["name"].startswith("_Z")}

    lists = sorted(glob.glob(os.path.join(abi_dir, "*.abilist")))
    assert lists
    reference_list = os.path.join(abi_dir, "x86_64-unknown-linux-gnu.libcxxabi.v1.stable.exceptions.nonew.abilist")
    reference = defined_cxx(load(reference_list))
    assert reference == {
        "_Z25handle_contract_violationRKNSt9contracts18contract_violationE",
        "_ZNKSt9contracts18contract_violation14detection_modeEv",
        "_ZNKSt9contracts18contract_violation14is_terminatingEv",
        "_ZNKSt9contracts18contract_violation20evaluation_exceptionEv",
        "_ZNKSt9contracts18contract_violation4kindEv",
        "_ZNKSt9contracts18contract_violation7commentEv",
        "_ZNKSt9contracts18contract_violation8locationEv",
        "_ZNKSt9contracts18contract_violation8semanticEv",
        "_ZNSt9contracts41invoke_default_contract_violation_handlerERKNS_18contract_violationE",
    }, reference

    okay = True

    def check(condition, path, message):
        global okay
        if not condition:
            okay = False
            print("{}: {}".format(os.path.basename(path), message))

    for path in lists:
        symbols = load(path)
        names = {s["name"] for s in symbols}
        defined = {s["name"] for s in symbols if s["is_defined"] and s["type"] != "I"}
        undefined = {s["name"] for s in symbols if not s["is_defined"]}
        reexported = {s["name"] for s in symbols if s["type"] == "I"}

        # The std::contracts symbols are the same everywhere: the ABI fixes
        # their names outside of the versioning namespace.
        cxx = defined_cxx(symbols)
        check(cxx == reference, path, "std::contracts symbols differ from the reference: missing {}, extra {}".format(
            sorted(reference - cxx), sorted(cxx - reference)))

        # libc++ uses the runtime's chain reader and default handler.
        for name in ("__cxa_find_field", "__contract_invoke_default_handler"):
            check(name in names, path, "does not mention {}".format(name))

        merged = "android" in os.path.basename(path)  # libc++abi is merged into libc++
        darwin = "apple-darwin" in os.path.basename(path)
        if merged:
            check(runtime_symbols - {"__handle_contract_violation"} <= defined, path,
                  "does not define the runtime: missing {}".format(
                      sorted(runtime_symbols - {"__handle_contract_violation"} - defined)))
            check("__handle_contract_violation" in undefined, path,
                  "the runtime's reference to the program's handler is missing")
        else:
            check(not (entry_points & defined), path, "defines the runtime's entry points, which belong to libc++abi")
            check({"__cxa_find_field", "__contract_invoke_default_handler"} <= undefined, path,
                  "does not reference the runtime's chain reader and default handler")
        if darwin:
            check(reexported == runtime_symbols, path,
                  "re-exports differ from libcxxabi/lib/contracts.exp: missing {}, extra {}".format(
                      sorted(runtime_symbols - reexported), sorted(reexported - runtime_symbols)))
        else:
            check(not reexported, path, "has re-exported symbols")

    assert okay
