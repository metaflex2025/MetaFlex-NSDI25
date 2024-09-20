cmake_minimum_required(VERSION 3.14)
project(nxframework-SRD LANGUAGES NONE)

find_package(nxFramework-fdk-Vanilla-u200-u250 REQUIRED)

add_subdirectory(cores)
add_subdirectory(config)
