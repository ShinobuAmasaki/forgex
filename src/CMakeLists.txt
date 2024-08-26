set(SHARED_LIB forgex)

add_subdirectory(essential)
add_subdirectory(nfa)
add_subdirectory(lazy_dfa)
add_subdirectory(ast)


add_library(${SHARED_LIB} SHARED
   api_internal_m.F90
   automaton_m.F90
   forgex.F90
   test_m.f90
)

target_link_libraries(forgex PRIVATE essential ast nfa lazy_dfa)

set_target_properties(${LIBRARY}
   PROPERTIES
   RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
   )

set_target_properties(${SHARED_LIB}
   PROPERTIES
   RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
   )

install (TARGETS ${SHARED_LIB}
   LIBRARY DESTINATION lib
   PUBLIC_HEADER DESTINATION include
   )

install (FILES
   ${CMAKE_CURRENT_BINARY_DIR}/forgex.mod
   DESTINATION lib
   )
