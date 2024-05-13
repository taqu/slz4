cmake_minimum_required(VERSION 3.2)

set(CMAKE_CONFIGURATION_TYPES "Debug" "Release")

set(ProjectName slz4)
project(${ProjectName})

include_directories(AFTER ${CMAKE_CURRENT_SOURCE_DIR})
set(THIRDPARTY_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/../thirdparty)

########################################################################
# LZ4
set(LZ4_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/lz4/lib)
set(LZ4_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/lz4/lib)
include_directories(AFTER ${LZ4_INCLUDE})
set(LZ4_HEADERS
    ${LZ4_INCLUDE}/lz4.h)
set(LZ4_SOURCES
    ${LZ4_SOURCE}/lz4.c)

source_group("include/lz4" FILES ${LZ4_HEADERS})
source_group("src/lz4" FILES ${LZ4_SOURCES})

########################################################################
# Sources
set(HEADERS "../slz4.h")
set(SOURCES "../slz4.cpp;main.cpp;test_slz4.cpp")

source_group("include" FILES ${HEADERS})
source_group("src" FILES ${SOURCES})

set(FILES ${HEADERS} ${SOURCES} ${LZ4_HEADERS} ${LZ4_SOURCES} ${XXHASH3_HEADERS} ${XXHASH3_SOURCES})

set(OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${OUTPUT_DIRECTORY}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${OUTPUT_DIRECTORY}")

add_executable(${ProjectName} ${FILES})

if(MSVC)
    set(DEFAULT_CXX_FLAGS "/DWIN32 /D_WINDOWS /D_MSBC /W4 /WX- /nologo /fp:precise /arch:AVX2 /Zc:wchar_t /TP /Gd")
    if("1800" VERSION_LESS MSVC_VERSION)
        set(DEFAULT_CXX_FLAGS "${DEFAULT_CXX_FLAGS} /EHsc")
    endif()

    if(MSVC_VERSION VERSION_LESS_EQUAL "1900")
        set(DEFAULT_CXX_FLAGS "${DEFAULT_CXX_FLAGS} /Zc:__cplusplus /std:c++latest")
    else()
        set(DEFAULT_CXX_FLAGS "${DEFAULT_CXX_FLAGS} /Zc:__cplusplus /std:c++20")
    endif()

    set(CMAKE_CXX_FLAGS "${DEFAULT_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS_DEBUG "/D_DEBUG /MDd /Zi /Ob0 /Od /RTC1 /Gy /GR- /GS /Gm-")
    set(CMAKE_CXX_FLAGS_RELEASE "/MD /O2 /GL /GR- /DNDEBUG")

elseif(UNIX)
    set(DEFAULT_CXX_FLAGS "-Wall -O2 -std=c++20 -march=x86-64-v3 -fno-exceptions")
    set(CMAKE_CXX_FLAGS "${DEFAULT_CXX_FLAGS}")
elseif(APPLE)
endif()

set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT ${ProjectName})
set_target_properties(${ProjectName}
    PROPERTIES
        OUTPUT_NAME_DEBUG "${ProjectName}" OUTPUT_NAME_RELEASE "${ProjectName}"
        VS_DEBUGGER_WORKING_DIRECTORY "${OUTPUT_DIRECTORY}")

