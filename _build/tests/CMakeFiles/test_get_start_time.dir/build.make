# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.17

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/rmcd/anaconda3/envs/pyprms/bin/cmake

# The command to remove a file.
RM = /home/rmcd/anaconda3/envs/pyprms/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/rmcd/git/bmi-prms6-streamflow

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/rmcd/git/bmi-prms6-streamflow/_build

# Include any dependencies generated for this target.
include tests/CMakeFiles/test_get_start_time.dir/depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/test_get_start_time.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/test_get_start_time.dir/flags.make

tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o: tests/CMakeFiles/test_get_start_time.dir/flags.make
tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o: ../tests/test_get_start_time.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/rmcd/git/bmi-prms6-streamflow/_build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/rmcd/git/bmi-prms6-streamflow/tests/test_get_start_time.f90 -o CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o

tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.i"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/rmcd/git/bmi-prms6-streamflow/tests/test_get_start_time.f90 > CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.i

tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.s"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/rmcd/git/bmi-prms6-streamflow/tests/test_get_start_time.f90 -o CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.s

tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.o: tests/CMakeFiles/test_get_start_time.dir/flags.make
tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.o: ../tests/fixtures.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/rmcd/git/bmi-prms6-streamflow/_build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.o"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/rmcd/git/bmi-prms6-streamflow/tests/fixtures.f90 -o CMakeFiles/test_get_start_time.dir/fixtures.f90.o

tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/test_get_start_time.dir/fixtures.f90.i"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/rmcd/git/bmi-prms6-streamflow/tests/fixtures.f90 > CMakeFiles/test_get_start_time.dir/fixtures.f90.i

tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/test_get_start_time.dir/fixtures.f90.s"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && /home/rmcd/anaconda3/envs/pyprms/bin/x86_64-conda_cos6-linux-gnu-gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/rmcd/git/bmi-prms6-streamflow/tests/fixtures.f90 -o CMakeFiles/test_get_start_time.dir/fixtures.f90.s

# Object files for target test_get_start_time
test_get_start_time_OBJECTS = \
"CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o" \
"CMakeFiles/test_get_start_time.dir/fixtures.f90.o"

# External object files for target test_get_start_time
test_get_start_time_EXTERNAL_OBJECTS =

tests/test_get_start_time: tests/CMakeFiles/test_get_start_time.dir/test_get_start_time.f90.o
tests/test_get_start_time: tests/CMakeFiles/test_get_start_time.dir/fixtures.f90.o
tests/test_get_start_time: tests/CMakeFiles/test_get_start_time.dir/build.make
tests/test_get_start_time: src/libbmiprmsstreamflow.so
tests/test_get_start_time: /home/rmcd/anaconda3/envs/pyprms/lib/libprmslib.so
tests/test_get_start_time: /home/rmcd/anaconda3/envs/pyprms/lib/libcoretran.so
tests/test_get_start_time: /home/rmcd/anaconda3/envs/pyprms/lib/libnetcdf.so
tests/test_get_start_time: /home/rmcd/anaconda3/envs/pyprms/lib/libnetcdff.so
tests/test_get_start_time: tests/CMakeFiles/test_get_start_time.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/rmcd/git/bmi-prms6-streamflow/_build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking Fortran executable test_get_start_time"
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/test_get_start_time.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/test_get_start_time.dir/build: tests/test_get_start_time

.PHONY : tests/CMakeFiles/test_get_start_time.dir/build

tests/CMakeFiles/test_get_start_time.dir/clean:
	cd /home/rmcd/git/bmi-prms6-streamflow/_build/tests && $(CMAKE_COMMAND) -P CMakeFiles/test_get_start_time.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/test_get_start_time.dir/clean

tests/CMakeFiles/test_get_start_time.dir/depend:
	cd /home/rmcd/git/bmi-prms6-streamflow/_build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/rmcd/git/bmi-prms6-streamflow /home/rmcd/git/bmi-prms6-streamflow/tests /home/rmcd/git/bmi-prms6-streamflow/_build /home/rmcd/git/bmi-prms6-streamflow/_build/tests /home/rmcd/git/bmi-prms6-streamflow/_build/tests/CMakeFiles/test_get_start_time.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/test_get_start_time.dir/depend

