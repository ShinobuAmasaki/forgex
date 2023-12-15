!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!

module forgex
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   
   use :: syntax_tree_m, only: parse_regex, tree_t, print_tree
   use :: nfa_m, only: build_nfa, print_nfa

   ! use :: dfa_m, only: convert_nfa_to_dfa, print_dfa, matching
   implicit none

contains


end module forgex