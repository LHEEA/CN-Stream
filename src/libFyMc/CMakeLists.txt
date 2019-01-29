set(
    FYMCPACK_SOURCES
        src/mfpGlobal.f90
        src/general/mfpGeneral.f90
        src/geometry/mfpGeometry.f90
)

add_library(fymc STATIC ${FYMCPACK_SOURCES})
target_include_directories(fymc PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

install(TARGETS fymc DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
