# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.17

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build

src/CMakeFiles/bmiprmsstreamflow.dir/bmi.f90.o.provides.build: src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp
src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp: src/CMakeFiles/bmiprmsstreamflow.dir/bmi.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod src/bmif_2_0.mod src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp GNU
src/CMakeFiles/bmiprmsstreamflow.dir/bmi.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch src/CMakeFiles/bmiprmsstreamflow.dir/bmi.f90.o.provides.build
src/CMakeFiles/bmiprmsstreamflow.dir/build: src/CMakeFiles/bmiprmsstreamflow.dir/bmi.f90.o.provides.build

src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/bmif_2_0.mod.stamp
src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o: src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.mod.stamp
src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o.provides.build: src/CMakeFiles/bmiprmsstreamflow.dir/bmiprmsstreamflow.mod.stamp
src/CMakeFiles/bmiprmsstreamflow.dir/bmiprmsstreamflow.mod.stamp: src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod src/bmiprmsstreamflow.mod src/CMakeFiles/bmiprmsstreamflow.dir/bmiprmsstreamflow.mod.stamp GNU
src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o.provides.build
src/CMakeFiles/bmiprmsstreamflow.dir/build: src/CMakeFiles/bmiprmsstreamflow.dir/bmi_prms_streamflow.f90.o.provides.build

src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/control_class.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/prms_basin.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/prms_constants.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/prms_muskingum.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/simulation_class.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o: /home/rmcd/anaconda3/envs/pyprms/include/coretran/variablekind.mod
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o.provides.build: src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.mod.stamp
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.mod.stamp: src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod src/m_prms_streamflow.mod src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.mod.stamp GNU
src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o.provides.build
src/CMakeFiles/bmiprmsstreamflow.dir/build: src/CMakeFiles/bmiprmsstreamflow.dir/m_prms_streamflow.f90.o.provides.build
