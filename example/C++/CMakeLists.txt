add_executable(prog
  src/commCNS.cpp
  src/main.cpp)
set_target_properties(prog PROPERTIES LINKER_LANGUAGE CXX)
include_directories(src)
target_link_libraries(prog CNStream)
