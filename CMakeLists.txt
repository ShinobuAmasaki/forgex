cmake_minimum_required(VERSION 3.0)

enable_language(Fortran)

project(forgex
   VERSION 1.0
   DESCRIPTION "Fortran Regular Expression"
   LANGUAGES Fortran
)

message("Project: ${PROJECT_NAME}")
message("Description: ${PROJECT_DESCRIPTION}")


add_subdirectory(src)

add_executable(test001 test/test_case_001.f90)

target_sources(test001
   PRIVATE
   src/test_m.f90
   )

target_include_directories(test001 PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)

target_link_libraries(test001 PRIVATE forgex)

enable_testing()

add_test(
   NAME Forgex_test_case_001 COMMAND test001
)