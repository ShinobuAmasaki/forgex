set(SHARED_LIB forgex)

add_library(${SHARED_LIB}
   SHARED
   api_internal_m.f90
   automaton_m.f90
   enums_m.f90
   lazy_dfa_graph_m.f90
   lazy_dfa_node_m.f90
   nfa_graph_m.f90
   nfa_node_m.f90
   parameters_m.f90
   priority_queue_m.f90
   segment_disjoin_m.f90
   segment_m.f90
   sort_m.f90
   syntax_tree_m.f90
   utf8_m.f90
   forgex.f90
)


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
