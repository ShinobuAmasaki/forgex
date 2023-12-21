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
   
   use :: syntax_tree_m
   use :: automaton_m
   implicit none
   private

   public :: operator(.in.)
   public :: operator(.match.)

   interface operator(.in.)
      module procedure :: in__matching
   end interface

   interface operator(.match.)
      module procedure :: match__matching
   end interface 

   type(automaton_t) :: automaton
   type(automaton_t) :: cache
contains

   function in__matching (pattern, str) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32) :: from, to
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from = 0
      to = 0

      root => build_syntax_tree(tape, pattern)
! call print_tree(root)
      call cache%init()
      call cache%build_nfa(root)
! call cache%print_nfa()
      call cache%convert_NFA_to_DFA()
! call cache%print_dfa()
      call cache%matching(str, from, to)

! write(stderr, *) from, to

      if (from  > 0 .and. to > 0) then
         res = .true.
      else
         res = .false.
      end if

      call deallocate_tree(root)

      ! call cache%deallocate_automaton()

   end function in__matching
   

   function match__matching(pattern, str) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32) :: from, to
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from = 0
      to = 0

      root => build_syntax_tree(tape, pattern)
! call print_tree(root)
      call cache%init()
      call cache%build_nfa(root)
! call cache%print_nfa()
      call cache%convert_NFA_to_DFA()
! call cache%print_dfa()
      res = cache%matching_exactly(str)

! write(stderr, *) from, to

      call deallocate_tree(root)
      ! call cache%deallocate_automaton()

   end function match__matching


end module forgex