#----------------------------------------------------------------
# Generated CMake target import file.
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "bmiprmsstreamflow" for configuration ""
set_property(TARGET bmiprmsstreamflow APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)
set_target_properties(bmiprmsstreamflow PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_NOCONFIG "/home/rmcd/anaconda3/envs/pyprms/lib/libprmslib.so;/home/rmcd/anaconda3/envs/pyprms/lib/libcoretran.so;/home/rmcd/anaconda3/envs/pyprms/lib/libnetcdf.so;/home/rmcd/anaconda3/envs/pyprms/lib/libnetcdff.so"
  IMPORTED_LOCATION_NOCONFIG "${_IMPORT_PREFIX}/lib/libbmiprmsstreamflow.so"
  IMPORTED_SONAME_NOCONFIG "libbmiprmsstreamflow.so"
  )

list(APPEND _IMPORT_CHECK_TARGETS bmiprmsstreamflow )
list(APPEND _IMPORT_CHECK_FILES_FOR_bmiprmsstreamflow "${_IMPORT_PREFIX}/lib/libbmiprmsstreamflow.so" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
