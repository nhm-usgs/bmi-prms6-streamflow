# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.17

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build

tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.f90.o.provides.build: tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.mod.stamp
tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.mod.stamp: tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod tests/fixtures.mod tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.mod.stamp GNU
tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.f90.o.provides.build
tests/CMakeFiles/test_get_grid_face_nodes.dir/build: tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.f90.o.provides.build

tests/CMakeFiles/test_get_grid_face_nodes.dir/test_get_grid_face_nodes.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp
tests/CMakeFiles/test_get_grid_face_nodes.dir/test_get_grid_face_nodes.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/bmiprmsstreamflow.mod.stamp
tests/CMakeFiles/test_get_grid_face_nodes.dir/test_get_grid_face_nodes.f90.o: tests/CMakeFiles/test_get_grid_face_nodes.dir/fixtures.mod.stamp
