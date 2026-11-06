cmake_minimum_required(VERSION 3.16)
project(wpsim_io LANGUAGES Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    add_compile_options(-Wall -Wextra -fcheck=all -fbacktrace
                        -fconvert=big-endian -frecord-marker=4 -ffree-line-length-none)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    add_compile_options(-warn -check -traceback -extend-source -convert big_endian)
endif()

add_library(wpsim_io STATIC
    src/wpsim_constants.F90
    src/wpsim_fields.F90
    src/wpsim_files.F90
    src/wpsim_io.F90
    src/wpsim_projection.F90
)

target_include_directories(wpsim_io PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})

add_executable(read_wpsim_example  examples/read_wpsim_example.F90)
target_link_libraries(read_wpsim_example  PRIVATE wpsim_io)

add_executable(write_wpsim_example examples/write_wpsim_example.F90)
target_link_libraries(write_wpsim_example PRIVATE wpsim_io)

enable_testing()
add_test(NAME write_wpsim_example
         COMMAND write_wpsim_example
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR})

add_test(NAME read_wpsim_example
         COMMAND read_wpsim_example
         WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})