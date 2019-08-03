add_executable(main
  modComm_CN_Stream.f90
  modVariablesRF.f90
  CreateInputDict.f90
  main.f90
  )
target_link_libraries(main dl)

install(TARGETS main
        RUNTIME DESTINATION bin)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/Computation/RF_input.dict
    ${CMAKE_CURRENT_BINARY_DIR}/RF_input.dict
    COPYONLY)

add_test(NAME test_fortran
         COMMAND main
         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

if(${CMAKE_BUILD_TYPE_UPPER} STREQUAL "COVERAGE")
    add_coverage(main)
endif()