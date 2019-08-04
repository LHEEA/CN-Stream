set(
    CNSTREAM_SOURCES
        HOS_modlinear_wave.f90
        HOS_modmaths.f90
        lib_CN_Stream.f90
        modCNinitialize.f90
        modMatrix.f90
        modModal.f90
        modReconstruction.f90
        modReconstrucVol.f90
        modSetupNameList.f90
        modSolve.f90
        modType.f90
        modUtils.f90
        modOutputs.f90
        modVariablesRF.f90
)

set(
    CNSTREAM_MAIN_SOURCES
        main_CN_Stream.f90
)

set(CNSTREAM_FLAG
    fymc
)

set(CNSTREAM_INCLUDE
    ${CMAKE_Fortran_MODULE_DIRECTORY}
)

add_library(CNStream SHARED ${CNSTREAM_SOURCES})

target_include_directories(CNStream PRIVATE ${CNSTREAM_INCLUDE})
target_link_libraries(CNStream ${CNSTREAM_FLAG})

set_target_properties(CNStream
    PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CNSTREAM_LIB_OUT}
)

install(TARGETS CNStream
        LIBRARY DESTINATION ${CNSTREAM_LIB_OUT}
)

## ------------- COMPILE MAIN  ---------------------

add_executable(mainCNS ${CNSTREAM_MAIN_SOURCES} ${CNSTREAM_SOURCES})

target_include_directories(mainCNS
    PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}
)

target_link_libraries(mainCNS fymc)

install(TARGETS mainCNS
        RUNTIME DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
)

add_test(NAME test_mainCNS
         COMMAND mainCNS
         WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})

if(${CMAKE_BUILD_TYPE_UPPER} STREQUAL "COVERAGE")
    add_coverage(mainCNS)
endif()
