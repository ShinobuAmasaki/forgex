set(STATIC_LIB forgex)

add_subdirectory(essential)
add_subdirectory(nfa)
add_subdirectory(lazy_dfa)
add_subdirectory(ast)


add_library(${STATIC_LIB} STATIC
   api_internal_m.F90
   automaton_m.F90
   forgex.F90
)

set_target_properties(${STATIC_LIB} PROPERTIES
   Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files
)

target_link_libraries(forgex PRIVATE essential ast nfa lazy_dfa)
target_link_directories(forgex PRIVATE ${CMAKE_BINARY_DIR}/src/essential)
target_link_directories(forgex PRIVATE ${CMAKE_BINARY_DIR}/src/ast)
target_link_directories(forgex PRIVATE ${CMAKE_BINARY_DIR}/src/nfa)
target_link_directories(forgex PRIVATE ${CMAKE_BINARY_DIR}/src/lazy_dfa)

target_include_directories(forgex PUBLIC $<TARGET_PROPERTY:${STATIC_LIB},Fortran_MODULE_DIRECTORY>)
target_include_directories(forgex PUBLIC $<TARGET_PROPERTY:essential,Fortran_MODULE_DIRECTORY>)
target_include_directories(forgex PUBLIC $<TARGET_PROPERTY:ast,Fortran_MODULE_DIRECTORY>)
target_include_directories(forgex PUBLIC $<TARGET_PROPERTY:nfa,Fortran_MODULE_DIRECTORY>)
target_include_directories(forgex PUBLIC $<TARGET_PROPERTY:lazy_dfa,Fortran_MODULE_DIRECTORY>)

set_target_properties(${LIBRARY}
   PROPERTIES
   RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
   )

set_target_properties(${STATIC_LIB}
   PROPERTIES
   RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
   )

install (TARGETS ${STATIC_LIB} essential ast nfa lazy_dfa
   EXPORT "${STATIC_LIB}"
   LIBRARY DESTINATION lib
   PUBLIC_HEADER DESTINATION include
   )

install (FILES
   ${CMAKE_CURRENT_BINARY_DIR}/forgex.mod
   DESTINATION lib
   )

install (EXPORT ${STATIC_LIB}
   NAMESPACE "${PROJECT_NAME}::"
   DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
   )
