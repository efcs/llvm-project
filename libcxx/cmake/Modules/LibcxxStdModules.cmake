set(INCLUDE_PATH "/tmp/shit/include/c++/v1/" )
set(CONFIG_INCLUDE_PATH "/tmp/shit/include/x86_64-unknown-linux-gnu/c++/v1"))

target_compile_options(std.compat
  PUBLIC
    -nostdinc++
    -Wno-reserved-module-identifier
    -Wno-reserved-user-defined-literal
	-fmodule-file=std=${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/std.dir/std.pcm
    @LIBCXX_COMPILE_FLAGS@
)
set_target_properties(std.compat
  PROPERTIES
    OUTPUT_NAME   "c++std.compat"
)
add_dependencies(std.compat std)

if(NOT TARGET std)
    add_library(std INTERFACE)
    target_compile_features(std INTERFACE cxx_std_23)
    target_sources(std INTERFACE
        FILE_SET cxx_modules TYPE CXX_MODULES FILES
            "${INCLUDE_PATH}/__modules/std.cppm"
    )
    target_include_directories(std SYSTEM INTERFACE
        ${INCLUDE_PATH}
        ${CONFIG_INCLUDE_PATH}
    )
    target_compile_options(std INTERFACE
        -nostdinc++
        -Wno-reserved-module-identifier
        -Wno-reserved-user-defined-literal
    )
    set_target_properties(std PROPERTIES
        OUTPUT_NAME "c++std"
    )
endif()

if(NOT TARGET std.compat)
    add_library(std.compat INTERFACE)
    target_compile_features(std.compat INTERFACE cxx_std_23)
    target_sources(std.compat INTERFACE
        FILE_SET cxx_modules TYPE CXX_MODULES FILES
            "${INCLUDE_PATH}/__modules/std.compat.cppm"
    )
    target_include_directories(std SYSTEM INTERFACE
        ${INCLUDE_PATH}
        ${CONFIG_INCLUDE_PATH}
    )
    target_compile_options(std.compat INTERFACE
        -nostdinc++
        -Wno-reserved-module-identifier
        -Wno-reserved-user-defined-literal
    )
    target_link_libraries(std.compat INTERFACE std)
    set_target_properties(std.compat PROPERTIES
        OUTPUT_NAME "c++std.compat"
    )
endif()
