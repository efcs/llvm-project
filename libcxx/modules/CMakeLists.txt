
# Install the CompileLibcxxModule.cmake file in the correct location
install(FILES ${CMAKE_CURRENT_LIST_DIR}/CompileLibcxxModule.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/libc++
  COMPONENT cxx)
