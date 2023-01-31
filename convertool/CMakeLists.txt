cmake_minimum_required(VERSION 3.5)


if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING
      "Choose the type of build, options are: Debug Release
RelWithDebInfo MinSizeRel."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)

project(convertool LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# change accordingly
include_directories("/usr/local/include")
include_directories("../convertool/orpp")


add_executable(convertool main.cpp)

#

#target_link_libraries("seirfilter" ${LNLOPT} )


