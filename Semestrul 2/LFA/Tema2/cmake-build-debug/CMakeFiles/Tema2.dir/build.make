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


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

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
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.17.1/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.17.1/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2"

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug"

# Include any dependencies generated for this target.
include CMakeFiles/Tema2.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Tema2.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Tema2.dir/flags.make

CMakeFiles/Tema2.dir/main.cpp.o: CMakeFiles/Tema2.dir/flags.make
CMakeFiles/Tema2.dir/main.cpp.o: ../main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/Tema2.dir/main.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Tema2.dir/main.cpp.o -c "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/main.cpp"

CMakeFiles/Tema2.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Tema2.dir/main.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/main.cpp" > CMakeFiles/Tema2.dir/main.cpp.i

CMakeFiles/Tema2.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Tema2.dir/main.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/main.cpp" -o CMakeFiles/Tema2.dir/main.cpp.s

CMakeFiles/Tema2.dir/src/nfa.cpp.o: CMakeFiles/Tema2.dir/flags.make
CMakeFiles/Tema2.dir/src/nfa.cpp.o: ../src/nfa.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/Tema2.dir/src/nfa.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Tema2.dir/src/nfa.cpp.o -c "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/nfa.cpp"

CMakeFiles/Tema2.dir/src/nfa.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Tema2.dir/src/nfa.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/nfa.cpp" > CMakeFiles/Tema2.dir/src/nfa.cpp.i

CMakeFiles/Tema2.dir/src/nfa.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Tema2.dir/src/nfa.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/nfa.cpp" -o CMakeFiles/Tema2.dir/src/nfa.cpp.s

CMakeFiles/Tema2.dir/src/dfa.cpp.o: CMakeFiles/Tema2.dir/flags.make
CMakeFiles/Tema2.dir/src/dfa.cpp.o: ../src/dfa.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/Tema2.dir/src/dfa.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Tema2.dir/src/dfa.cpp.o -c "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/dfa.cpp"

CMakeFiles/Tema2.dir/src/dfa.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Tema2.dir/src/dfa.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/dfa.cpp" > CMakeFiles/Tema2.dir/src/dfa.cpp.i

CMakeFiles/Tema2.dir/src/dfa.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Tema2.dir/src/dfa.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/dfa.cpp" -o CMakeFiles/Tema2.dir/src/dfa.cpp.s

CMakeFiles/Tema2.dir/src/reggram.cpp.o: CMakeFiles/Tema2.dir/flags.make
CMakeFiles/Tema2.dir/src/reggram.cpp.o: ../src/reggram.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object CMakeFiles/Tema2.dir/src/reggram.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Tema2.dir/src/reggram.cpp.o -c "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/reggram.cpp"

CMakeFiles/Tema2.dir/src/reggram.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Tema2.dir/src/reggram.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/reggram.cpp" > CMakeFiles/Tema2.dir/src/reggram.cpp.i

CMakeFiles/Tema2.dir/src/reggram.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Tema2.dir/src/reggram.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/reggram.cpp" -o CMakeFiles/Tema2.dir/src/reggram.cpp.s

CMakeFiles/Tema2.dir/src/regex.cpp.o: CMakeFiles/Tema2.dir/flags.make
CMakeFiles/Tema2.dir/src/regex.cpp.o: ../src/regex.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object CMakeFiles/Tema2.dir/src/regex.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Tema2.dir/src/regex.cpp.o -c "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/regex.cpp"

CMakeFiles/Tema2.dir/src/regex.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Tema2.dir/src/regex.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/regex.cpp" > CMakeFiles/Tema2.dir/src/regex.cpp.i

CMakeFiles/Tema2.dir/src/regex.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Tema2.dir/src/regex.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/src/regex.cpp" -o CMakeFiles/Tema2.dir/src/regex.cpp.s

# Object files for target Tema2
Tema2_OBJECTS = \
"CMakeFiles/Tema2.dir/main.cpp.o" \
"CMakeFiles/Tema2.dir/src/nfa.cpp.o" \
"CMakeFiles/Tema2.dir/src/dfa.cpp.o" \
"CMakeFiles/Tema2.dir/src/reggram.cpp.o" \
"CMakeFiles/Tema2.dir/src/regex.cpp.o"

# External object files for target Tema2
Tema2_EXTERNAL_OBJECTS =

Tema2: CMakeFiles/Tema2.dir/main.cpp.o
Tema2: CMakeFiles/Tema2.dir/src/nfa.cpp.o
Tema2: CMakeFiles/Tema2.dir/src/dfa.cpp.o
Tema2: CMakeFiles/Tema2.dir/src/reggram.cpp.o
Tema2: CMakeFiles/Tema2.dir/src/regex.cpp.o
Tema2: CMakeFiles/Tema2.dir/build.make
Tema2: CMakeFiles/Tema2.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir="/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles" --progress-num=$(CMAKE_PROGRESS_6) "Linking CXX executable Tema2"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Tema2.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Tema2.dir/build: Tema2

.PHONY : CMakeFiles/Tema2.dir/build

CMakeFiles/Tema2.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Tema2.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Tema2.dir/clean

CMakeFiles/Tema2.dir/depend:
	cd "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug" && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2" "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2" "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug" "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug" "/Users/dariusbuhai/Desktop/Proiecte Facultate/Semestrul 2/LFA/Tema2/cmake-build-debug/CMakeFiles/Tema2.dir/DependInfo.cmake" --color=$(COLOR)
.PHONY : CMakeFiles/Tema2.dir/depend

