


macro(compile_std_module)
  set(options WRAP_HEADERS)
  set(oneValueArgs TARGET INCLUDE_DIRECTORY TARGET_INCLUDE_DIRECTORY)
  set(multiValueArgs INCLUDE_DIRECTORIES COMPILE_FLAGS)
  cmake_parse_arguments(COMPILE_STD_MODULE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # The target sources must be relative to this file, so we copy the module file to the build directory
  # and use a relative path to it.
  # It would be nice if we could use the out-of-tree sources, but for this reason
  # it's easier to use a single-file module definition rather than a tree of files.
  file(COPY ${COMPILE_STD_MODULE_INCLUDE_DIRECTORY}/__modules/std.cppm DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  file(COPY ${COMPILE_STD_MODULE_INCLUDE_DIRECTORY}/__modules/std.compat.cppm DESTINATION ${CMAKE_CURRENT_BINARY_DIR})



  add_library(${COMPILE_STD_MODULE_TARGET})

  # This is the magic that creates the module
  target_sources(${COMPILE_STD_MODULE_TARGET}
    PUBLIC FILE_SET cxx_modules TYPE CXX_MODULES FILES
      ${CMAKE_CURRENT_BINARY_DIR}/std.cppm
      ${CMAKE_CURRENT_BINARY_DIR}/std.compat.cppm
  )

  if (COMPILE_STD_MODULE_WRAP_HEADERS)
    set(WRAP_HEADERS_FLAG "SHELL:-isystem /usr/local/include/c++/vm")
  endif()

  target_compile_options(${COMPILE_STD_MODULE_TARGET}
    PRIVATE
      -nostdinc++
      "SHELL:-isystem ${COMPILE_STD_MODULE_INCLUDE_DIRECTORY}"
      "SHELL:-isystem ${COMPILE_STD_MODULE_TARGET_INCLUDE_DIRECTORY}"
      -Wno-reserved-module-identifier
      -Wno-reserved-user-defined-literal
  )
  target_compile_options(${COMPILE_STD_MODULE_TARGET}
    PUBLIC
      "SHELL:-isystem ${COMPILE_STD_MODULE_TARGET_INCLUDE_DIRECTORY}"
      ${COMPILE_STD_MODULE_COMPILE_FLAGS}
  )
  target_link_options(${COMPILE_STD_MODULE_TARGET}
    PRIVATE
     -stdlib=libc++
     # FIXME: Link the module file
  )
  target_compile_options(${COMPILE_STD_MODULE_TARGET}
    PUBLIC
      -fprebuilt-module-path=${CMAKE_CURRENT_BINARY_DIR}
  )
  set_target_properties(${COMPILE_STD_MODULE_TARGET}
    PROPERTIES
      OUTPUT_NAME   "c++${COMPILE_STD_MODULE_MODULE}${CMAKE_STD_MODULE_TARGET}"
  )


  # TODO: Remove this and replace it simply with the target provided by the user
  # This is a test to try wrapping the C++ standard library headers and replacing
  # them with headers containing only `import std;` (and an include for version, __config)
  add_library(${COMPILE_STD_MODULE_TARGET}_wrappers INTERFACE)
  target_compile_options(${COMPILE_STD_MODULE_TARGET}_wrappers INTERFACE -nostdinc++ "SHELL:-isystem /usr/local/include/c++/vm" "SHELL:-isystem  ${COMPILE_STD_MODULE_TARGET_INCLUDE_DIRECTORY}" -fprebuilt-module-path=${CMAKE_CURRENT_BINARY_DIR})
  target_link_options(${COMPILE_STD_MODULE_TARGET}_wrappers INTERFACE  -stdlib=libc++  -fprebuilt-module-path=${CMAKE_CURRENT_BINARY_DIR})
  target_link_libraries(${COMPILE_STD_MODULE_TARGET}_wrappers INTERFACE ${COMPILE_STD_MODULE_TARGET})
  add_dependencies(${COMPILE_STD_MODULE_TARGET}_wrappers ${COMPILE_STD_MODULE_TARGET})


  #add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-nostdinc++>)
  #add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:SHELL:-isystem \"${COMPILE_STD_MODULE_INCLUDE_DIRECTORY}\">")
  #add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:SHELL:-isystem \"${COMPILE_STD_MODULE_TARGET_INCLUDE_DIRECTORY}\">")
  #add_link_options($<$<LINK_LANGUAGE:CXX>:-stdlib=libc++>)
endmacro()

# Example usage
# Allow the user to compile the stdmodule for a particular header installation
# under their own name and with their own flags.
#
# compile_std_module(TARGET "mystd" MODULE "std"
#    INCLUDE_DIRECTORY "<install-prefix>/include/c++/v1" # Used to find __modules/<MODULE>.cppm
#    TARGET_INCLUDE_DIRECTORY "</if/needed/x86_64-unknown-linux-gnu>/c++/v1" # If needed for __config_site
#    COMPILE_FLAGS "-fno-exceptions" # ADditional compile flags to use when compiling the module
# )
