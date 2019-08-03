###-------------------------------------------------------------------------###

cmake_minimum_required(VERSION 2.7)  #... Specify the minimum version for CMake

###-------------------------------------------------------------------------###

if(NOT WIN32)
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(ColourBold  "${Esc}[1m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
  set(BoldRed     "${Esc}[1;31m")
  set(BoldGreen   "${Esc}[1;32m")
  set(BoldYellow  "${Esc}[1;33m")
  set(BoldBlue    "${Esc}[1;34m")
  set(BoldMagenta "${Esc}[1;35m")
  set(BoldCyan    "${Esc}[1;36m")
  set(BoldWhite   "${Esc}[1;37m")
endif()

###-------------------------------------------------------------------------###

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
set(COMPILE_LANGUAGE Fortran CXX)
if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER gfortran)       #... Fortran Compiler Path
  # set(CMAKE_Fortran_COMPILER ifort)        #... Fortran Compiler Path
endif()

###-------------------------------------------------------------------------###

set(PROJECT_NAME CNStream)                         #... Project Name
set(PROJECT_COMPILE_PATH ${CMAKE_HOME_DIRECTORY})   #... COMPILE_PATH (RF Directory where CMakeLists.txt exists)

project(${PROJECT_NAME} ${COMPILE_LANGUAGE})        # PROJECT NAME

###-------------------------------------------------------------------------###

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_COMPILE_PATH})
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_COMPILE_PATH}/lib)

#option(BUILD_OF_LIB "Build <libCNStream.so> in $FOAM_USER_LIBBIN" OFF)

set(CNSTREAM_LIB_OUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "-fPIC -O3 -xHOST -ipo -ip -O0 -traceback -check all -CB -check uninit -g")
  #set(CMAKE_Fortran_FLAGS "-O3 -xHOST -ipo -ip -O0 -traceback -check all -warn all")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "-fPIC -O3 -march=corei7 -msse2 -funroll-loops -fno-protect-parens -ffast-math -fcheck=bounds")
endif()

# In case you need to compile shared library in openFoam. Uncomment the belows.
#   cmake -H. -Bbuild -DBUILD_OF_LIB=ON

 if (BUILD_OF_LIB)

     if (NOT "$ENV{FOAM_USER_LIBBIN}" STREQUAL "")
         set(CNSTREAM_LIB_OUT $ENV{FOAM_USER_LIBBIN})
     else()
         message("${BoldRed} ")
         message("   [ERROR] Not in OpenFOAM Environment... ")
         message("${ColourReset} ${Green}")
         message("   Wrong $FOAM_USER_LIBBIN path")
         message("")
         message("       $FOAM_USER_LIBBIN : " $ENV{FOAM_USER_LIBBIN})
         message("${ColourReset} ")
        message( FATAL_ERROR )
    endif()
endif()

message("${Yellow}")
message("    CNSTREAM LIBRARY OUT PATH ")
message("${Green}")
message("       - CNSTREAM_LIB_OUT  : " ${CNSTREAM_LIB_OUT})
message("${ColourReset}")
###-------------------------------------------------------------------------###

#... -fPIC is needed to generate shared library
#set(CMAKE_Fortran_FLAGS "-O3 -march=corei7 -msse2 -funroll-loops -fno-protect-parens -ffast-math -fPIC")

MESSAGE(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_UPPER)
IF(CMAKE_BUILD_TYPE_UPPER MATCHES DEBUG)
    SET(BUILD_SHARED_LIBS OFF)
ENDIF()

IF(${CMAKE_BUILD_TYPE_UPPER} STREQUAL "COVERAGE")
    MESSAGE(STATUS "Adding coverage")
    FIND_PACKAGE(codecov)
ENDIF()

add_subdirectory(src/libFyMc)
add_subdirectory(src/libCNStream)
ENABLE_TESTING()
add_subdirectory(example)
INCLUDE(CMakePack)
