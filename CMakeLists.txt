cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

project(forgex
   VERSION 3.4
   DESCRIPTION "Fortran Regular Expression"
   LANGUAGES Fortran
)

message("Project: ${PROJECT_NAME}")
message("Description: ${PROJECT_DESCRIPTION}")


add_subdirectory(src)


### TESTS settings 

set(TEST_API_DIR test/test_api)
set(TEST_AST_DIR test/test_ast)

set(TEST_API_CASES
   test_case_001
   test_case_002
   test_case_003
   test_case_004
   test_case_005
   test_case_006
   test_case_007
)

set(TEST_AST_CASES
   ast_case_001
   ast_case_002
)

foreach(TEST ${TEST_API_CASES})
   add_executable(${TEST} ${TEST_API_DIR}/${TEST}.f90)
   target_sources(${TEST} PRIVATE src/test_m.f90)
   target_include_directories(${TEST} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)
   target_link_libraries(${TEST} PRIVATE forgex)
   add_test(NAME Forgex_${TEST} COMMAND ${TEST})
endforeach()

foreach(TEST ${TEST_AST_CASES})
   add_executable(${TEST} ${TEST_AST_DIR}/${TEST}.f90)
   target_sources(${TEST} PRIVATE src/test_m.f90)
   target_include_directories(${TEST} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/src)
   target_link_libraries(${TEST} PRIVATE forgex)
   add_test(NAME Forgex_${TEST} COMMAND ${TEST})
endforeach()

enable_testing()
