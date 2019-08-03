# Install the dependencies of the program
IF(MINGW)
    IF(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
        SET(PLATFORM_ABI x64)
    ELSE()
        SET(PLATFORM_ABI x86)
    ENDIF()
    GET_FILENAME_COMPONENT(MINGW_BIN_PATH ${CMAKE_C_COMPILER} PATH)
    MESSAGE(STATUS "MINGW BIN PATH = ${MINGW_BIN_PATH}")
    SET(MINGW_LIB_DIRECTORY "")
    IF(${PLATFORM_ABI} MATCHES "x64")
        SET(MINGW_LIB_DIRECTORY "x86_64-w64-mingw32")
    ELSE()
        SET(MINGW_LIB_DIRECTORY "i686-w64-mingw32")
    ENDIF()
    IF(EXISTS "${MINGW_BIN_PATH}/../${MINGW_LIB_DIRECTORY}/")
        FILE(GLOB MINGW_DLLS ${MINGW_BIN_PATH}/../${MINGW_LIB_DIRECTORY}/lib/*.dll)
    ELSE()
        # Maybe Dlls are present in the mingw bin directory
        FILE(GLOB MINGW_DLLS ${MINGW_BIN_PATH}/*.dll)
    ENDIF()
    IF(NOT MINGW_DLLS)
        MESSAGE(FATAL_ERROR "No MinGW DLL was found. CMake could not find the MinGW dll of your installation")
    ENDIF()
    SET(MINGW_GCC_EXCEPTION "")
    FOREACH(f ${MINGW_DLLS})
        IF(${f} MATCHES "libgcc")
            MESSAGE(STATUS "MINGW libgcc library = ${f}")
            IF(${f} MATCHES "dw2")
                SET(MINGW_GCC_EXCEPTION "dw2")
            ELSEIF(${f} MATCHES "sjlj")
                SET(MINGW_GCC_EXCEPTION "sjlj")
            ELSEIF(${f} MATCHES "seh")
                SET(MINGW_GCC_EXCEPTION "seh")
            ELSE()
                MESSAGE(FATAL_ERROR "MinGW exception was not identified amongst dw2, seh, sjlj")
            ENDIF()
        ENDIF()
    ENDFOREACH()
    IF(${MINGW_GCC_EXCEPTION} STREQUAL "" )
        MESSAGE(STATUS "MinGW exception mechanism could not be found")
        MESSAGE(FATAL_ERROR "Please update your version of MinGW")
    ENDIF()
    FOREACH(f ${MINGW_DLLS})
        IF(EXISTS ${f})
            INSTALL(FILES ${f} DESTINATION ${LIBRARY_OUTPUT_DIRECTORY})
        ENDIF()
    ENDFOREACH()
ENDIF()

################################################################################
# Instructions to build an installer
IF(UNIX OR MSYS)
    INCLUDE(InstallRequiredSystemLibraries)
    SET(CPACK_PACKAGE_CONTACT "Guillaume Ducrozet (guillaume.ducrozet@ec-nantes.fr)")
    SET(CPACK_PACKAGE_NAME "${PROJECT_NAME}_installer")
    SET(CPACK_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION OFF)
    SET(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)
    SET(CPACK_COMPONENT_INCLUDE_TOPLEVEL_DIRECTORY OFF)
    SET(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}")
    SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${PROJECT_NAME}")
    SET(CPACK_PACKAGE_VENDOR "")
    SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE.txt")
    SET(CPACK_PACKAGE_VERSION_MAJOR "1")
    SET(CPACK_PACKAGE_VERSION_MINOR "0")
    SET(CPACK_PACKAGE_VERSION_PATCH "0")
    #SET(CPACK_STRIP_FILES ON)
    IF(WIN32)
        # There is a bug in NSIS that does not handle full unix paths properly. Make
        # sure there is at least one set of four (4) backlasshes.
        #SET(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
        SET(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}")
        SET(CPACK_NSIS_INSTALLED_ICON_NAME "${PROJECT_NAME}_installer")
        SET(CPACK_NSIS_DISPLAY_NAME "${PROJECT_NAME}")
        SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.ec-nantes.fr")
        SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.ec-nantes.fr")
        SET(CPACK_NSIS_CONTACT "guillaume.ducrozet@ec-nantes.fr")
        SET(CPACK_NSIS_MODIFY_PATH OFF)
        MESSAGE(STATUS "CPACK_NSIS_INSTALL_ROOT : ${CPACK_NSIS_INSTALL_ROOT}")
        MESSAGE(STATUS "CPACK_PACKAGE_INSTALL_DIRECTORY :${CPACK_PACKAGE_INSTALL_DIRECTORY}")
    ELSE(WIN32)
        SET(CPACK_STRIP_FILES "./${PROJECT_NAME}_installer")
        SET(CPACK_SOURCE_STRIP_FILES "")
        IF(NOT CPACK_GENERATOR)
            IF(APPLE)
                # SET(CPACK_GENERATOR "DragNDrop")
                SET(CPACK_GENERATOR "ZIP")
            ELSEIF(UNIX)
                SET(CPACK_GENERATOR "DEB")
                SET(CPACK_PACKAGE_EXECUTABLES "${PROJECT_NAME}" "${PROJECT_NAME}")
                SET(CPACK_PACKAGING_INSTALL_PREFIX "$HOME/${PROJECT_NAME}")
            ELSE()
                SET(CPACK_GENERATOR "ZIP")
            ENDIF()
        ENDIF()
        ##CPACK_PACKAGING_INSTALL_PREFIX : Sets the default root that the generated package installs into, '/usr' is the default for the debian and redhat generators
        SET(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}")
    ENDIF()
    INCLUDE(CPack)
ENDIF()