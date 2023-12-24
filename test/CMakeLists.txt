set(TEST_001 test_case_001)
set(TEST_002 test_case_002)
set(TEST_003 test_case_003)


add_executable(${TEST_001}
   test_case_001.f90
   )

target_link_libraries(${TEST_001}
   PRIVATE
   ${SHARED_LIB}
   )

