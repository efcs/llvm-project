if(NOT TARGET std)
    add_library(std INTERFACE)
    target_compile_features(std INTERFACE cxx_std_23)
    target_sources(std INTERFACE
        FILE_SET cxx_modules TYPE CXX_MODULES FILES
            "${CMAKE_CURRENT_LIST_DIR}/std.cppm"
    )
    target_include_directories(std SYSTEM INTERFACE
        "${LIBCXX_GENERATED_INCLUDE_TARGET_DIR}"
        "${LIBCXX_GENERATED_INCLUDE_DIR}"
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
            "${CMAKE_CURRENT_LIST_DIR}/std.compat.cppm"
    )
    target_include_directories(std.compat SYSTEM INTERFACE
        "${LIBCXX_GENERATED_INCLUDE_TARGET_DIR}"
        "${LIBCXX_GENERATED_INCLUDE_DIR}"
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
