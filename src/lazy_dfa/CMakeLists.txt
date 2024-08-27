add_library(lazy_dfa STATIC
   lazy_dfa_graph_m.F90
   lazy_dfa_node_m.F90
)

target_link_libraries(lazy_dfa PUBLIC essential nfa)

set_target_properties(lazy_dfa PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files
)