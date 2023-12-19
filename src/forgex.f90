!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!
!!     forgex_m defines APIs of Forgex.
!!  
module forgex
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   
   use :: syntax_tree_m, only: parse_regex, tree_t, print_tree, deallocate_tree
   use :: automaton_m
   use :: in_operator_m

   type(automaton_t) :: automaton

contains


end module forgex