
set(CMAKE_C_COMPILER "/opt/llvm-tot/bin/clang" CACHE STRING "")
set(CMAKE_CXX_COMPILER "/opt/llvm-tot/bin/clang++" CACHE STRING "")
set(LLVM_ENABLE_PROJECTS "libcxx;libcxxabi;libunwind" CACHE STRING "")
set(LIBCXX_CXX_ABI "default" CACHE STRING "")
set(CMAKE_BUILD_TYPE "RELWITHDEBINFO" CACHE STRING "")
