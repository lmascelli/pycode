cmake_minimum_required(VERSION 3.23)

project(pycode_h5 LANGUAGES C)

add_library(pycode_h5 STATIC
  "pycode_h5.h" "pycode_h5.c"
)
set(HDF5_INCLUDE_DIR $ENV{HDF5_INCLUDE_DIR})
target_include_directories(pycode_h5 PUBLIC ${HDF5_INCLUDE_DIR})

install(TARGETS pycode_h5)
