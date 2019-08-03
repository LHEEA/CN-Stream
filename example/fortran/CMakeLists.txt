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
    ${PROJECT_SOURCE_DIR}/lib/RF_input.dict
    COPYONLY)

add_test(NAME test_fortran
         COMMAND main
         WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set_property(TEST test_fortran PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=${PROJECT_SOURCE_DIR}/lib")

if(${CMAKE_BUILD_TYPE_UPPER} STREQUAL "COVERAGE")
    add_coverage(main)
endif()