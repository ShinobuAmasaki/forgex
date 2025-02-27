add_library(essential STATIC
   enums_m.f90
   error_m.f90
   parameters_m.f90
   priority_queue_m.f90
   segment_m.F90
   segment_disjoin_m.F90
   sort_m.f90
   utf8_m.f90
   utility_m.f90
)

set_target_properties(essential PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files
)