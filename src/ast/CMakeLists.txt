add_library(ast STATIC
   syntax_tree_node_m.F90
   syntax_tree_graph_m.F90
   syntax_tree_optimize_m.F90
)

set_target_properties(ast PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files
)

target_link_libraries(ast PUBLIC essential)
target_include_directories(ast PUBLIC $<TARGET_PROPERTY:essential,Fortran_MODULE_DIRECTORY>)