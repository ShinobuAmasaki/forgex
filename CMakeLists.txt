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


### TESTS settings 

set(TEST001 test_case_001)
set(TEST002 test_case_002)
set(TEST003 test_case_003)

add_executable(${TEST001} test/test_case_001.f90)
add_executable(${TEST002} test/test_case_002.f90)
add_executable(${TEST003} test/test_case_003.f90)

target_sources(${TEST001} PRIVATE src/test_m.f90)
target_sources(${TEST002} PRIVATE src/test_m.f90)
target_sources(${TEST003} PRIVATE src/test_m.f90)

target_include_directories(${TEST001} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)
target_include_directories(${TEST002} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)
target_include_directories(${TEST003} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)


target_link_libraries(${TEST001} PRIVATE forgex)
target_link_libraries(${TEST002} PRIVATE forgex)
target_link_libraries(${TEST003} PRIVATE forgex)

enable_testing()

add_test(
   NAME Forgex_test_case_001 COMMAND ${TEST001}
)


add_test(
   NAME Forgex_test_case_002 COMMAND ${TEST002}
)

add_test(
   NAME Forgex_test_case_003 COMMAND ${TEST003}
)