add_library(nfa SHARED
   nfa_node_m.F90
   nfa_graph_m.F90
   nfa_state_set_m.F90
)

set_target_properties(nfa PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files
)

target_link_libraries(nfa PUBLIC essential ast)
target_include_directories(nfa PUBLIC $<TARGET_PROPERTY:essential,Fortran_MODULE_DIRECTORY>)
target_include_directories(nfa PUBLIC $<TARGET_PROPERTY:ast,Fortran_MODULE_DIRECTORY>)