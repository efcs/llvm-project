# See docs/CMake.html for instructions about how to build LLVM with CMake.

cmake_minimum_required(VERSION 3.4.3)

if(POLICY CMP0068)
  cmake_policy(SET CMP0068 NEW)
  set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)
endif()

if(POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif()

if(POLICY CMP0077)
  cmake_policy(SET CMP0077 NEW)
endif()

if(NOT DEFINED LLVM_VERSION_MAJOR)
  set(LLVM_VERSION_MAJOR 11)
endif()
if(NOT DEFINED LLVM_VERSION_MINOR)
  set(LLVM_VERSION_MINOR 0)
endif()
if(NOT DEFINED LLVM_VERSION_PATCH)
  set(LLVM_VERSION_PATCH 0)
endif()
if(NOT DEFINED LLVM_VERSION_SUFFIX)
  set(LLVM_VERSION_SUFFIX git)
endif()

if (NOT PACKAGE_VERSION)
  set(PACKAGE_VERSION
    "${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}.${LLVM_VERSION_PATCH}${LLVM_VERSION_SUFFIX}")
endif()

if ((CMAKE_GENERATOR MATCHES "Visual Studio") AND (CMAKE_GENERATOR_TOOLSET STREQUAL ""))
  message(WARNING "Visual Studio generators use the x86 host compiler by "
                  "default, even for 64-bit targets. This can result in linker "
                  "instability and out of memory errors. To use the 64-bit "
                  "host compiler, pass -Thost=x64 on the CMake command line.")
endif()

if (CMAKE_GENERATOR STREQUAL "Xcode" AND NOT CMAKE_OSX_ARCHITECTURES)
  # Some CMake features like object libraries get confused if you don't
  # explicitly specify an architecture setting with the Xcode generator.
  set(CMAKE_OSX_ARCHITECTURES "x86_64")
endif()

project(LLVM
  VERSION ${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}.${LLVM_VERSION_PATCH}
  LANGUAGES C CXX ASM)

set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, default to Debug")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Build type (default Debug)" FORCE)
endif()

# Side-by-side subprojects layout: automatically set the
# LLVM_EXTERNAL_${project}_SOURCE_DIR using LLVM_ALL_PROJECTS
# This allows an easy way of setting up a build directory for llvm and another
# one for llvm+clang+... using the same sources.
set(LLVM_ALL_PROJECTS "clang;clang-tools-extra;compiler-rt;debuginfo-tests;libc;libclc;libcxx;libcxxabi;libunwind;lld;lldb;mlir;openmp;parallel-libs;polly;pstl")
set(LLVM_ENABLE_PROJECTS "" CACHE STRING
	"Semicolon-separated list of projects to build (${LLVM_ALL_PROJECTS}), or \"all\".")
if( LLVM_ENABLE_PROJECTS STREQUAL "all" )
  set( LLVM_ENABLE_PROJECTS ${LLVM_ALL_PROJECTS})
endif()

# LLVM_ENABLE_PROJECTS_USED is `ON` if the user has ever used the
# `LLVM_ENABLE_PROJECTS` CMake cache variable.  This exists for
# several reasons:
#
# * As an indicator that the `LLVM_ENABLE_PROJECTS` list is now the single
# source of truth for which projects to build. This means we will ignore user
# supplied `LLVM_TOOL_<project>_BUILD` CMake cache variables and overwrite
# them.
#
# * The case where the user previously had `LLVM_ENABLE_PROJECTS` set to a
# non-empty list but now the user wishes to disable building all other projects
# by setting `LLVM_ENABLE_PROJECTS` to an empty string. In that case we still
# need to set the `LLVM_TOOL_${upper_proj}_BUILD` variables so that we disable
# building all the projects that were previously enabled.
set(LLVM_ENABLE_PROJECTS_USED OFF CACHE BOOL "")
mark_as_advanced(LLVM_ENABLE_PROJECTS_USED)

if (LLVM_ENABLE_PROJECTS_USED OR NOT LLVM_ENABLE_PROJECTS STREQUAL "")
  set(LLVM_ENABLE_PROJECTS_USED ON CACHE BOOL "" FORCE)
  foreach(proj ${LLVM_ALL_PROJECTS} ${LLVM_EXTERNAL_PROJECTS})
    string(TOUPPER "${proj}" upper_proj)
    string(REGEX REPLACE "-" "_" upper_proj ${upper_proj})
    if ("${proj}" IN_LIST LLVM_ENABLE_PROJECTS)
      message(STATUS "${proj} project is enabled")
      set(SHOULD_ENABLE_PROJECT TRUE)
      set(PROJ_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../${proj}")
      if(NOT EXISTS "${PROJ_DIR}" OR NOT IS_DIRECTORY "${PROJ_DIR}")
        message(FATAL_ERROR "LLVM_ENABLE_PROJECTS requests ${proj} but directory not found: ${PROJ_DIR}")
      endif()
      if( LLVM_EXTERNAL_${upper_proj}_SOURCE_DIR STREQUAL "" )
        set(LLVM_EXTERNAL_${upper_proj}_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../${proj}" CACHE PATH "" FORCE)
      else()
        set(LLVM_EXTERNAL_${upper_proj}_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../${proj}" CACHE PATH "")
      endif()
    elseif ("${proj}" IN_LIST LLVM_EXTERNAL_PROJECTS)
      message(STATUS "${proj} project is enabled")
      set(SHOULD_ENABLE_PROJECT TRUE)
    else()
      message(STATUS "${proj} project is disabled")
      set(SHOULD_ENABLE_PROJECT FALSE)
    endif()
    # Force `LLVM_TOOL_${upper_proj}_BUILD` variables to have values that
    # corresponds with `LLVM_ENABLE_PROJECTS`. This prevents the user setting
    # `LLVM_TOOL_${upper_proj}_BUILD` variables externally. At some point
    # we should deprecate allowing users to set these variables by turning them
    # into normal CMake variables rather than cache variables.
    set(LLVM_TOOL_${upper_proj}_BUILD
      ${SHOULD_ENABLE_PROJECT}
      CACHE
      BOOL "Whether to build ${upper_proj} as part of LLVM" FORCE
    )
  endforeach()
endif()
unset(SHOULD_ENABLE_PROJECT)

# Build llvm with ccache if the package is present
set(LLVM_CCACHE_BUILD OFF CACHE BOOL "Set to ON for a ccache enabled build")
if(LLVM_CCACHE_BUILD)
  find_program(CCACHE_PROGRAM ccache)
  if(CCACHE_PROGRAM)
      set(LLVM_CCACHE_MAXSIZE "" CACHE STRING "Size of ccache")
      set(LLVM_CCACHE_DIR "" CACHE STRING "Directory to keep ccached data")
      set(LLVM_CCACHE_PARAMS "CCACHE_CPP2=yes CCACHE_HASHDIR=yes"
          CACHE STRING "Parameters to pass through to ccache")

      set(CCACHE_PROGRAM "${LLVM_CCACHE_PARAMS} ${CCACHE_PROGRAM}")
      if (LLVM_CCACHE_MAXSIZE)
        set(CCACHE_PROGRAM "CCACHE_MAXSIZE=${LLVM_CCACHE_MAXSIZE} ${CCACHE_PROGRAM}")
      endif()
      if (LLVM_CCACHE_DIR)
        set(CCACHE_PROGRAM "CCACHE_DIR=${LLVM_CCACHE_DIR} ${CCACHE_PROGRAM}")
      endif()
      set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${CCACHE_PROGRAM})
  else()
    message(FATAL_ERROR "Unable to find the program ccache. Set LLVM_CCACHE_BUILD to OFF")
  endif()
endif()

option(LLVM_DEPENDENCY_DEBUGGING "Dependency debugging mode to verify correctly expressed library dependencies (Darwin only)" OFF)

# Some features of the LLVM build may be disallowed when dependency debugging is
# enabled. In particular you cannot use ccache because we want to force compile
# operations to always happen.
if(LLVM_DEPENDENCY_DEBUGGING)
  if(NOT CMAKE_HOST_APPLE)
    message(FATAL_ERROR "Dependency debugging is only currently supported on Darwin hosts.")
  endif()
  if(LLVM_CCACHE_BUILD)
    message(FATAL_ERROR "Cannot enable dependency debugging while using ccache.")
  endif()
endif()

option(LLVM_ENABLE_DAGISEL_COV "Debug: Prints tablegen patterns that were used for selecting" OFF)
option(LLVM_ENABLE_GISEL_COV "Enable collection of GlobalISel rule coverage" OFF)
if(LLVM_ENABLE_GISEL_COV)
  set(LLVM_GISEL_COV_PREFIX "${CMAKE_BINARY_DIR}/gisel-coverage-" CACHE STRING "Provide a filename prefix to collect the GlobalISel rule coverage")
endif()

# Add path for custom modules
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules"
  )

# Generate a CompilationDatabase (compile_commands.json file) for our build,
# for use by clang_complete, YouCompleteMe, etc.
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

option(LLVM_INSTALL_BINUTILS_SYMLINKS
  "Install symlinks from the binutils tool names to the corresponding LLVM tools." OFF)

option(LLVM_INSTALL_CCTOOLS_SYMLINKS
  "Install symlinks from the cctools tool names to the corresponding LLVM tools." OFF)

option(LLVM_INSTALL_UTILS "Include utility binaries in the 'install' target." OFF)

option(LLVM_INSTALL_TOOLCHAIN_ONLY "Only include toolchain files in the 'install' target." OFF)

# Unfortunatly Clang is too eager to search directories for module maps, which can cause the
# installed version of the maps to be found when building LLVM from source. Therefore we turn off
# the installation by default. See llvm.org/PR31905.
option(LLVM_INSTALL_MODULEMAPS "Install the modulemap files in the 'install' target." OFF)

option(LLVM_USE_FOLDERS "Enable solution folders in Visual Studio. Disable for Express versions." ON)
if ( LLVM_USE_FOLDERS )
  set_property(GLOBAL PROPERTY USE_FOLDERS ON)
endif()

include(VersionFromVCS)

option(LLVM_APPEND_VC_REV
  "Embed the version control system revision in LLVM" ON)

set(PACKAGE_NAME LLVM)
set(PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
set(PACKAGE_BUGREPORT "https://bugs.llvm.org/")

set(BUG_REPORT_URL "${PACKAGE_BUGREPORT}" CACHE STRING
  "Default URL where bug reports are to be submitted.")

# Configure CPack.
set(CPACK_PACKAGE_INSTALL_DIRECTORY "LLVM")
set(CPACK_PACKAGE_VENDOR "LLVM")
set(CPACK_PACKAGE_VERSION_MAJOR ${LLVM_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${LLVM_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${LLVM_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION ${PACKAGE_VERSION})
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.TXT")
set(CPACK_NSIS_COMPRESSOR "/SOLID lzma \r\n SetCompressorDictSize 32")
if(WIN32 AND NOT UNIX)
  set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "LLVM")
  set(CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}\\\\cmake\\\\nsis_logo.bmp")
  set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}\\\\cmake\\\\nsis_icon.ico")
  set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}\\\\cmake\\\\nsis_icon.ico")
  set(CPACK_NSIS_MODIFY_PATH "ON")
  set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL "ON")
  if( CMAKE_CL_64 )
    set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
  endif()
endif()
include(CPack)

# Sanity check our source directory to make sure that we are not trying to
# generate an in-source build (unless on MSVC_IDE, where it is ok), and to make
# sure that we don't have any stray generated files lying around in the tree
# (which would end up getting picked up by header search, instead of the correct
# versions).
if( CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR AND NOT MSVC_IDE )
  message(FATAL_ERROR "In-source builds are not allowed.
Please create a directory and run cmake from there, passing the path
to this source directory as the last argument.
This process created the file `CMakeCache.txt' and the directory `CMakeFiles'.
Please delete them.")
endif()

string(TOUPPER "${CMAKE_BUILD_TYPE}" uppercase_CMAKE_BUILD_TYPE)

if (CMAKE_BUILD_TYPE AND
    NOT uppercase_CMAKE_BUILD_TYPE MATCHES "^(DEBUG|RELEASE|RELWITHDEBINFO|MINSIZEREL)$")
  message(FATAL_ERROR "Invalid value for CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
endif()

set(LLVM_LIBDIR_SUFFIX "" CACHE STRING "Define suffix of library directory name (32/64)" )

set(LLVM_TOOLS_INSTALL_DIR "bin" CACHE STRING "Path for binary subdirectory (defaults to 'bin')")
mark_as_advanced(LLVM_TOOLS_INSTALL_DIR)

set(LLVM_UTILS_INSTALL_DIR "${LLVM_TOOLS_INSTALL_DIR}" CACHE STRING
    "Path to install LLVM utilities (enabled by LLVM_INSTALL_UTILS=ON) (defaults to LLVM_TOOLS_INSTALL_DIR)")
mark_as_advanced(LLVM_UTILS_INSTALL_DIR)

# They are used as destination of target generators.
set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib${LLVM_LIBDIR_SUFFIX})
if(WIN32 OR CYGWIN)
  # DLL platform -- put DLLs into bin.
  set(LLVM_SHLIB_OUTPUT_INTDIR ${LLVM_RUNTIME_OUTPUT_INTDIR})
else()
  set(LLVM_SHLIB_OUTPUT_INTDIR ${LLVM_LIBRARY_OUTPUT_INTDIR})
endif()

# Each of them corresponds to llvm-config's.
set(LLVM_TOOLS_BINARY_DIR ${LLVM_RUNTIME_OUTPUT_INTDIR}) # --bindir
set(LLVM_LIBRARY_DIR      ${LLVM_LIBRARY_OUTPUT_INTDIR}) # --libdir
set(LLVM_MAIN_SRC_DIR     ${CMAKE_CURRENT_SOURCE_DIR}  ) # --src-root
set(LLVM_MAIN_INCLUDE_DIR ${LLVM_MAIN_SRC_DIR}/include ) # --includedir
set(LLVM_BINARY_DIR       ${CMAKE_CURRENT_BINARY_DIR}  ) # --prefix

# Note: LLVM_CMAKE_PATH does not include generated files
set(LLVM_CMAKE_PATH ${LLVM_MAIN_SRC_DIR}/cmake/modules)
set(LLVM_EXAMPLES_BINARY_DIR ${LLVM_BINARY_DIR}/examples)
set(LLVM_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)


option(BUILD_SHARED_LIBS
  "Build all libraries as shared libraries instead of static" OFF)

option(LLVM_ENABLE_BACKTRACES "Enable embedding backtraces on crash." ON)
if(LLVM_ENABLE_BACKTRACES)
  set(ENABLE_BACKTRACES 1)
endif()

option(LLVM_ENABLE_UNWIND_TABLES "Emit unwind tables for the libraries" ON)

option(LLVM_ENABLE_CRASH_OVERRIDES "Enable crash overrides." ON)
if(LLVM_ENABLE_CRASH_OVERRIDES)
  set(ENABLE_CRASH_OVERRIDES 1)
endif()

option(LLVM_ENABLE_CRASH_DUMPS "Turn on memory dumps on crashes. Currently only implemented on Windows." OFF)

option(LLVM_ENABLE_FFI "Use libffi to call external functions from the interpreter" OFF)
set(FFI_LIBRARY_DIR "" CACHE PATH "Additional directory, where CMake should search for libffi.so")
set(FFI_INCLUDE_DIR "" CACHE PATH "Additional directory, where CMake should search for ffi.h or ffi/ffi.h")

set(LLVM_TARGET_ARCH "host"
  CACHE STRING "Set target to use for LLVM JIT or use \"host\" for automatic detection.")

option(LLVM_ENABLE_TERMINFO "Use terminfo database if available." ON)

set(LLVM_ENABLE_LIBXML2 "ON" CACHE STRING "Use libxml2 if available. Can be ON, OFF, or FORCE_ON")

option(LLVM_ENABLE_LIBEDIT "Use libedit if available." ON)

option(LLVM_ENABLE_LIBPFM "Use libpfm for performance counters if available." ON)

option(LLVM_ENABLE_THREADS "Use threads if available." ON)



option(LLVM_ENABLE_PIC "Build Position-Independent Code" ON)
option(LLVM_ENABLE_WARNINGS "Enable compiler warnings." ON)
option(LLVM_ENABLE_MODULES "Compile with C++ modules enabled." OFF)
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  option(LLVM_ENABLE_MODULE_DEBUGGING "Compile with -gmodules." ON)
  option(LLVM_ENABLE_LOCAL_SUBMODULE_VISIBILITY "Compile with -fmodules-local-submodule-visibility." OFF)
else()
  option(LLVM_ENABLE_MODULE_DEBUGGING "Compile with -gmodules." OFF)
  option(LLVM_ENABLE_LOCAL_SUBMODULE_VISIBILITY "Compile with -fmodules-local-submodule-visibility." ON)
endif()
option(LLVM_ENABLE_LIBCXX "Use libc++ if available." OFF)
option(LLVM_STATIC_LINK_CXX_STDLIB "Statically link the standard library." OFF)
option(LLVM_ENABLE_LLD "Use lld as C and C++ linker." OFF)
option(LLVM_ENABLE_PEDANTIC "Compile with pedantic enabled." ON)
option(LLVM_ENABLE_WERROR "Fail and stop if a warning is triggered." OFF)

option(LLVM_ENABLE_DUMP "Enable dump functions even when assertions are disabled" OFF)

if( NOT uppercase_CMAKE_BUILD_TYPE STREQUAL "DEBUG" )
  option(LLVM_ENABLE_ASSERTIONS "Enable assertions" OFF)
else()
  option(LLVM_ENABLE_ASSERTIONS "Enable assertions" ON)
endif()

option(LLVM_ENABLE_EXPENSIVE_CHECKS "Enable expensive checks" OFF)

# While adding scalable vector support to LLVM, we temporarily want to
# allow an implicit conversion of TypeSize to uint64_t. This CMake flag
# enables a more strict conversion where it asserts that the type is not
# a scalable vector type.
#
# Enabling this flag makes it easier to find cases where the compiler makes
# assumptions on the size being 'fixed size', when building tests for
# SVE/SVE2 or other scalable vector architectures.
option(LLVM_ENABLE_STRICT_FIXED_SIZE_VECTORS
       "Enable assertions that type is not scalable in implicit conversion from TypeSize to uint64_t" OFF)

set(LLVM_ABI_BREAKING_CHECKS "WITH_ASSERTS" CACHE STRING
  "Enable abi-breaking checks.  Can be WITH_ASSERTS, FORCE_ON or FORCE_OFF.")

option(LLVM_FORCE_USE_OLD_TOOLCHAIN
       "Set to ON to force using an old, unsupported host toolchain." OFF)

option(LLVM_TEMPORARILY_ALLOW_OLD_TOOLCHAIN
       "Set to ON to only warn when using a toolchain which is about to be deprecated, instead of emitting an error." OFF)

option(LLVM_USE_INTEL_JITEVENTS
  "Use Intel JIT API to inform Intel(R) VTune(TM) Amplifier XE 2011 about JIT code"
  OFF)


set(LLVM_USE_SANITIZER "" CACHE STRING
  "Define the sanitizer used to build binaries and tests.")
option(LLVM_OPTIMIZE_SANITIZED_BUILDS "Pass -O1 on debug sanitizer builds" ON)
set(LLVM_LIB_FUZZING_ENGINE "" CACHE PATH
  "Path to fuzzing library for linking with fuzz targets")

option(LLVM_USE_SPLIT_DWARF
  "Use -gsplit-dwarf when compiling llvm." OFF)

# Define an option controlling whether we should build for 32-bit on 64-bit
# platforms, where supported.
if( CMAKE_SIZEOF_VOID_P EQUAL 8 AND NOT (WIN32 OR ${CMAKE_SYSTEM_NAME} MATCHES "AIX"))
  # TODO: support other platforms and toolchains.
  option(LLVM_BUILD_32_BITS "Build 32 bits executables and libraries." OFF)
endif()

# Define the default arguments to use with 'lit', and an option for the user to
# override.
set(LIT_ARGS_DEFAULT "-sv")
if (MSVC_IDE OR XCODE)
  set(LIT_ARGS_DEFAULT "${LIT_ARGS_DEFAULT} --no-progress-bar")
endif()
set(LLVM_LIT_ARGS "${LIT_ARGS_DEFAULT}" CACHE STRING "Default options for lit")

# On Win32 hosts, provide an option to specify the path to the GnuWin32 tools.
if( WIN32 AND NOT CYGWIN )
  set(LLVM_LIT_TOOLS_DIR "" CACHE PATH "Path to GnuWin32 tools")
endif()

# Define options to control the inclusion and default build behavior for
# components which may not strictly be necessary (tools, examples, and tests).
#
# This is primarily to support building smaller or faster project files.
option(LLVM_INCLUDE_TOOLS "Generate build targets for the LLVM tools." ON)
option(LLVM_BUILD_TOOLS
  "Build the LLVM tools. If OFF, just generate build targets." ON)

option(LLVM_INCLUDE_UTILS "Generate build targets for the LLVM utils." ON)
option(LLVM_BUILD_UTILS
  "Build LLVM utility binaries. If OFF, just generate build targets." ON)

option(LLVM_INCLUDE_RUNTIMES "Generate build targets for the LLVM runtimes." ON)
option(LLVM_BUILD_RUNTIMES
  "Build the LLVM runtimes. If OFF, just generate build targets." ON)

option(LLVM_BUILD_RUNTIME
  "Build the LLVM runtime libraries." ON)
option(LLVM_BUILD_EXAMPLES
  "Build the LLVM example programs. If OFF, just generate build targets." OFF)
option(LLVM_INCLUDE_EXAMPLES "Generate build targets for the LLVM examples" ON)

if(LLVM_BUILD_EXAMPLES)
  add_definitions(-DBUILD_EXAMPLES)
endif(LLVM_BUILD_EXAMPLES)

option(LLVM_BUILD_TESTS
  "Build LLVM unit tests. If OFF, just generate build targets." OFF)
option(LLVM_INCLUDE_TESTS "Generate build targets for the LLVM unit tests." ON)
option(LLVM_INCLUDE_GO_TESTS "Include the Go bindings tests in test build targets." ON)

option(LLVM_BUILD_BENCHMARKS "Add LLVM benchmark targets to the list of default
targets. If OFF, benchmarks still could be built using Benchmarks target." OFF)
option(LLVM_INCLUDE_BENCHMARKS "Generate benchmark targets. If OFF, benchmarks can't be built." ON)

option (LLVM_BUILD_DOCS "Build the llvm documentation." OFF)
option (LLVM_INCLUDE_DOCS "Generate build targets for llvm documentation." ON)
option (LLVM_ENABLE_DOXYGEN "Use doxygen to generate llvm API documentation." OFF)
option (LLVM_ENABLE_SPHINX "Use Sphinx to generate llvm documentation." OFF)
option (LLVM_ENABLE_OCAMLDOC "Build OCaml bindings documentation." ON)
option (LLVM_ENABLE_BINDINGS "Build bindings." ON)

set(LLVM_INSTALL_DOXYGEN_HTML_DIR "share/doc/llvm/doxygen-html"
    CACHE STRING "Doxygen-generated HTML documentation install directory")
set(LLVM_INSTALL_OCAMLDOC_HTML_DIR "share/doc/llvm/ocaml-html"
    CACHE STRING "OCamldoc-generated HTML documentation install directory")

option (LLVM_BUILD_EXTERNAL_COMPILER_RT
  "Build compiler-rt as an external project." OFF)

option (LLVM_VERSION_PRINTER_SHOW_HOST_TARGET_INFO
  "Show target and host info when tools are invoked with --version." ON)

# You can configure which libraries from LLVM you want to include in the
# shared library by setting LLVM_DYLIB_COMPONENTS to a semi-colon delimited
# list of LLVM components. All component names handled by llvm-config are valid.
if(NOT DEFINED LLVM_DYLIB_COMPONENTS)
  set(LLVM_DYLIB_COMPONENTS "all" CACHE STRING
    "Semicolon-separated list of components to include in libLLVM, or \"all\".")
endif()

if(MSVC)
  option(LLVM_BUILD_LLVM_C_DYLIB "Build LLVM-C.dll (Windows only)" ON)
  # Set this variable to OFF here so it can't be set with a command-line
  # argument.
  set (LLVM_LINK_LLVM_DYLIB OFF)
  if (BUILD_SHARED_LIBS)
    message(FATAL_ERROR "BUILD_SHARED_LIBS options is not supported on Windows.")
  endif()
else()
  option(LLVM_LINK_LLVM_DYLIB "Link tools against the libllvm dynamic library" OFF)
  option(LLVM_BUILD_LLVM_C_DYLIB "Build libllvm-c re-export library (Darwin only)" OFF)
  set(LLVM_BUILD_LLVM_DYLIB_default OFF)
  if(LLVM_LINK_LLVM_DYLIB OR LLVM_BUILD_LLVM_C_DYLIB)
    set(LLVM_BUILD_LLVM_DYLIB_default ON)
  endif()
  option(LLVM_BUILD_LLVM_DYLIB "Build libllvm dynamic library" ${LLVM_BUILD_LLVM_DYLIB_default})
endif()

if (LLVM_LINK_LLVM_DYLIB AND BUILD_SHARED_LIBS)
  message(FATAL_ERROR "Cannot enable BUILD_SHARED_LIBS with LLVM_LINK_LLVM_DYLIB.  We recommend disabling BUILD_SHARED_LIBS.")
endif()

option(LLVM_OPTIMIZED_TABLEGEN "Force TableGen to be built with optimization" OFF)
if(CMAKE_CROSSCOMPILING OR (LLVM_OPTIMIZED_TABLEGEN AND (LLVM_ENABLE_ASSERTIONS OR CMAKE_CONFIGURATION_TYPES)))
  set(LLVM_USE_HOST_TOOLS ON)
endif()

if (MSVC_IDE)
  option(LLVM_ADD_NATIVE_VISUALIZERS_TO_SOLUTION "Configure project to use Visual Studio native visualizers" TRUE)
endif()

if (LLVM_BUILD_INSTRUMENTED OR LLVM_BUILD_INSTRUMENTED_COVERAGE OR
    LLVM_ENABLE_IR_PGO)
  if(NOT LLVM_PROFILE_MERGE_POOL_SIZE)
    # A pool size of 1-2 is probably sufficient on a SSD. 3-4 should be fine
    # for spining disks. Anything higher may only help on slower mediums.
    set(LLVM_PROFILE_MERGE_POOL_SIZE "4")
  endif()
  if(NOT LLVM_PROFILE_FILE_PATTERN)
    if(NOT LLVM_PROFILE_DATA_DIR)
      file(TO_NATIVE_PATH "${LLVM_BINARY_DIR}/profiles" LLVM_PROFILE_DATA_DIR)
    endif()
    file(TO_NATIVE_PATH "${LLVM_PROFILE_DATA_DIR}/%${LLVM_PROFILE_MERGE_POOL_SIZE}m.profraw" LLVM_PROFILE_FILE_PATTERN)
  endif()
  if(NOT LLVM_CSPROFILE_FILE_PATTERN)
    if(NOT LLVM_CSPROFILE_DATA_DIR)
      file(TO_NATIVE_PATH "${LLVM_BINARY_DIR}/csprofiles" LLVM_CSPROFILE_DATA_DIR)
    endif()
    file(TO_NATIVE_PATH "${LLVM_CSPROFILE_DATA_DIR}/%${LLVM_PROFILE_MERGE_POOL_SIZE}m.profraw" LLVM_CSPROFILE_FILE_PATTERN)
  endif()
endif()

if (LLVM_BUILD_STATIC)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
endif()

# Use libtool instead of ar if you are both on an Apple host, and targeting Apple.
if(CMAKE_HOST_APPLE AND APPLE)
  include(UseLibtool)
endif()

# Override the default target with an environment variable named by LLVM_TARGET_TRIPLE_ENV.
set(LLVM_TARGET_TRIPLE_ENV CACHE STRING "The name of environment variable to override default target. Disabled by blank.")
mark_as_advanced(LLVM_TARGET_TRIPLE_ENV)

set(LLVM_ENABLE_PER_TARGET_RUNTIME_DIR OFF CACHE BOOL
  "Enable per-target runtimes directory")

set(LLVM_PROFDATA_FILE "" CACHE FILEPATH
  "Profiling data file to use when compiling in order to improve runtime performance.")


# By default, we target the host, but this can be overridden at CMake
# invocation time.
set(LLVM_DEFAULT_TARGET_TRIPLE "${LLVM_HOST_TRIPLE}" CACHE STRING
  "Default target for which LLVM will generate code." )
set(TARGET_TRIPLE "${LLVM_DEFAULT_TARGET_TRIPLE}")
message(STATUS "LLVM host triple: ${LLVM_HOST_TRIPLE}")
message(STATUS "LLVM default target triple: ${LLVM_DEFAULT_TARGET_TRIPLE}")

if(WIN32 OR CYGWIN)
  if(BUILD_SHARED_LIBS OR LLVM_BUILD_LLVM_DYLIB)
    set(LLVM_ENABLE_PLUGINS_default ON)
  else()
    set(LLVM_ENABLE_PLUGINS_default OFF)
  endif()
else()
  set(LLVM_ENABLE_PLUGINS_default ${LLVM_ENABLE_PIC})
endif()
option(LLVM_ENABLE_PLUGINS "Enable plugin support" ${LLVM_ENABLE_PLUGINS_default})

include(HandleLLVMOptions)

include(FindPythonInterp)
if( NOT PYTHONINTERP_FOUND )
  message(FATAL_ERROR
"Unable to find Python interpreter, required for builds and testing.

Please install Python or specify the PYTHON_EXECUTABLE CMake variable.")
endif()

if( ${PYTHON_VERSION_STRING} VERSION_LESS 2.7 )
  message(FATAL_ERROR "Python 2.7 or newer is required")
endif()


######


# They are not referenced. See set_output_directory().
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${LLVM_BINARY_DIR}/bin )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${LLVM_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${LLVM_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )


include(AddLLVM)

add_subdirectory(utils/FileCheck)
add_subdirectory(utils/not)
# Work around a broken bfd ld behavior. When linking a binary with a
# foo.so library, it will try to find any library that foo.so uses and
# check its symbols. This is wasteful (the check was done when foo.so
# was created) and can fail since it is not the dynamic linker and
# doesn't know how to handle search paths correctly.
if (UNIX AND NOT APPLE AND NOT ${CMAKE_SYSTEM_NAME} MATCHES "SunOS|AIX")
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} -Wl,-allow-shlib-undefined")
endif()

set(CMAKE_INCLUDE_CURRENT_DIR ON)

if( ${CMAKE_SYSTEM_NAME} MATCHES SunOS )
   # special hack for Solaris to handle crazy system sys/regset.h
   include_directories("${LLVM_MAIN_INCLUDE_DIR}/llvm/Support/Solaris")
endif( ${CMAKE_SYSTEM_NAME} MATCHES SunOS )

# Make sure we don't get -rdynamic in every binary. For those that need it,
# use export_executable_symbols(target).
set(CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS "")


if( MINGW AND NOT "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang" )
  # People report that -O3 is unreliable on MinGW. The traditional
  # build also uses -O2 for that reason:
  llvm_replace_compiler_option(CMAKE_CXX_FLAGS_RELEASE "-O3" "-O2")
endif()

# Put this before tblgen. Else we have a circular dependence.




add_subdirectory(projects)


if( LLVM_INCLUDE_TESTS )
  if(EXISTS ${LLVM_MAIN_SRC_DIR}/projects/test-suite AND TARGET clang)
    include(LLVMExternalProjectUtils)
    llvm_ExternalProject_Add(test-suite ${LLVM_MAIN_SRC_DIR}/projects/test-suite
      USE_TOOLCHAIN
      EXCLUDE_FROM_ALL
      NO_INSTALL
      ALWAYS_CLEAN)
  endif()
  add_subdirectory(utils/lit)



  # Add a global check rule now that all subdirectories have been traversed
  # and we know the total set of lit testsuites.
  get_property(LLVM_LIT_TESTSUITES GLOBAL PROPERTY LLVM_LIT_TESTSUITES)
  get_property(LLVM_LIT_PARAMS GLOBAL PROPERTY LLVM_LIT_PARAMS)
  get_property(LLVM_LIT_DEPENDS GLOBAL PROPERTY LLVM_LIT_DEPENDS)
  get_property(LLVM_LIT_EXTRA_ARGS GLOBAL PROPERTY LLVM_LIT_EXTRA_ARGS)
  get_property(LLVM_ADDITIONAL_TEST_TARGETS
               GLOBAL PROPERTY LLVM_ADDITIONAL_TEST_TARGETS)
  get_property(LLVM_ADDITIONAL_TEST_DEPENDS
               GLOBAL PROPERTY LLVM_ADDITIONAL_TEST_DEPENDS)
  add_lit_target(check-all
    "Running all regression tests"
    ${LLVM_LIT_TESTSUITES}
    PARAMS ${LLVM_LIT_PARAMS}
    DEPENDS ${LLVM_LIT_DEPENDS} ${LLVM_ADDITIONAL_TEST_TARGETS}
    ARGS ${LLVM_LIT_EXTRA_ARGS}
    )
  if(TARGET check-runtimes)
    add_dependencies(check-all check-runtimes)
  endif()
  add_custom_target(test-depends
                    DEPENDS ${LLVM_LIT_DEPENDS} ${LLVM_ADDITIONAL_TEST_DEPENDS})
  set_target_properties(test-depends PROPERTIES FOLDER "Tests")
endif()


add_subdirectory(cmake/modules)

# Do this last so that all lit targets have already been created.
if (LLVM_INCLUDE_UTILS)
  add_subdirectory(utils/llvm-lit)
endif()
