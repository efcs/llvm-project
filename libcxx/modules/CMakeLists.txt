




configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/LibcxxModulesTargets.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/LibcxxModulesTargets.cmake"
    @ONLY
)

set(_all_modules)
list(APPEND _all_modules "${LIBCXX_GENERATED_MODULE_DIR}/CMakeLists.txt")
list(APPEND _all_modules "${LIBCXX_GENERATED_MODULE_DIR}/std.cppm")
list(APPEND _all_modules "${LIBCXX_GENERATED_MODULE_DIR}/std.compat.cppm")
foreach(file ${LIBCXX_MODULE_STD_SOURCES} ${LIBCXX_MODULE_STD_COMPAT_SOURCES})
  set(src "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
  set(dst "${LIBCXX_GENERATED_MODULE_DIR}/${file}")
  add_custom_command(OUTPUT ${dst}
    DEPENDS ${src}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${src} ${dst}
    COMMENT "Copying CXX module ${file}")
  list(APPEND _all_modules "${dst}")
endforeach()

add_custom_target(generate-cxx-modules
  ALL DEPENDS
    ${_all_modules}
)

# Configure the modules manifest.
# Use the relative path between the installation and the module in the json
# file. This allows moving the entire installation to a different location.
file(RELATIVE_PATH LIBCXX_MODULE_RELATIVE_PATH
  ${CMAKE_INSTALL_PREFIX}/${LIBCXX_INSTALL_LIBRARY_DIR}
  ${CMAKE_INSTALL_PREFIX}/${LIBCXX_INSTALL_MODULES_DIR})
configure_file(
  "modules.json.in"
  "${LIBCXX_LIBRARY_DIR}/libc++.modules.json"
  @ONLY
)

# Dummy library to make modules an installation component.
add_library(cxx-modules INTERFACE)
add_dependencies(cxx-modules generate-cxx-modules)

if (LIBCXX_INSTALL_MODULES)
  foreach(file ${LIBCXX_MODULE_STD_SOURCES} ${LIBCXX_MODULE_STD_COMPAT_SOURCES})
    get_filename_component(dir ${file} DIRECTORY)
    install(FILES ${file}
      DESTINATION "${LIBCXX_INSTALL_MODULES_DIR}/${dir}"
      COMPONENT cxx-modules
      PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
    )
  endforeach()

  # Install the generated module files.
  install(FILES
      "${LIBCXX_GENERATED_MODULE_DIR}/std.cppm"
      "${LIBCXX_GENERATED_MODULE_DIR}/std.compat.cppm"
    DESTINATION "${LIBCXX_INSTALL_MODULES_DIR}"
    COMPONENT cxx-modules
    PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
  )


    install(FILES  "${LIBCXX_GENERATED_MODULE_DIR}/LibcxxModulesConfig.cmake"
        DESTINATION "${LIBCXX_INSTALL_MODULES_DIR}/cmake/LibcxxModules"
        COMPONENT cxx-modules
        PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
    )

    install(FILES "${LIBCXX_GENERATED_MODULE_DIR}/LibcxxModulesTargets.cmake"
        DESTINATION "${LIBCXX_INSTALL_MODULES_DIR}/cmake/LibcxxModules"
        COMPONENT cxx-modules
        PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
    )

  # Install the module manifest.
  install(FILES
      "${LIBCXX_LIBRARY_DIR}/libc++.modules.json"
    DESTINATION "${LIBCXX_INSTALL_LIBRARY_DIR}"
    COMPONENT cxx-modules
    PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ
  )

  if (NOT CMAKE_CONFIGURATION_TYPES)
    add_custom_target(install-cxx-modules
                      DEPENDS cxx-modules
                      COMMAND "${CMAKE_COMMAND}"
                              -DCMAKE_INSTALL_COMPONENT=cxx-modules
                              -P "${CMAKE_BINARY_DIR}/cmake_install.cmake")
    # Stripping is a no-op for modules
    add_custom_target(install-cxx-modules-stripped DEPENDS install-cxx-modules)
  endif()
endif()
