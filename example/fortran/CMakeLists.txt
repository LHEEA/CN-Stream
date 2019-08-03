add_executable(main
  modComm_CN_Stream.f90
  modVariablesRF.f90
  CreateInputDict.f90
  main.f90
  )
target_link_libraries(main dl)

install(TARGETS main
        RUNTIME DESTINATION bin)

add_test(NAME test_fortran
         COMMAND main
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/Computation)
