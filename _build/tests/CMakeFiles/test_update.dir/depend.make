# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.17

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build

tests/CMakeFiles/test_update.dir/fixtures.f90.o.provides.build: tests/CMakeFiles/test_update.dir/fixtures.mod.stamp
tests/CMakeFiles/test_update.dir/fixtures.mod.stamp: tests/CMakeFiles/test_update.dir/fixtures.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod tests/fixtures.mod tests/CMakeFiles/test_update.dir/fixtures.mod.stamp GNU
tests/CMakeFiles/test_update.dir/fixtures.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch tests/CMakeFiles/test_update.dir/fixtures.f90.o.provides.build
tests/CMakeFiles/test_update.dir/build: tests/CMakeFiles/test_update.dir/fixtures.f90.o.provides.build

tests/CMakeFiles/test_update.dir/test_update.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp
tests/CMakeFiles/test_update.dir/test_update.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/bmiprmsstreamflow.mod.stamp
tests/CMakeFiles/test_update.dir/test_update.f90.o: tests/CMakeFiles/test_update.dir/fixtures.mod.stamp
