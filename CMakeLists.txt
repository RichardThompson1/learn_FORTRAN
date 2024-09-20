# cmake_minimum_required(VERSION 3.10)
# project(demo)
# enable_language(Fortran)
# set(CMAKE_Fortran_COMPILER "ifort")

# file(GLOB SRC_FILES "*.f90")

# add_executable(demo ${SRC_FILES})


cmake_minimum_required(VERSION 3.10)
project(demo Fortran)
enable_language(Fortran)
set(CMAKE_Fortran_COMPILER "ifort")

# List all source files
set(SRC_FILES main.f90 point_cartesian.f90 math_functions.f90)

# Create the executable
add_executable(demo ${SRC_FILES})

# Set the module output directory
set_target_properties(demo PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Include the module directory when compiling
target_include_directories(demo PRIVATE ${CMAKE_BINARY_DIR}/modules)
