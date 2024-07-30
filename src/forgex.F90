! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex module is a part of Forgex.
!
!! This file includes the API module of Forgex.
#ifdef IMPURE
#define pure
#endif
module forgex
   use :: forgex_syntax_tree_m, only: tree_node_t, tape_t, build_syntax_tree
#ifdef IMPURE
#ifdef DEBUG
   use :: forgex_syntax_tree_m, only: print_tree
#endif
#endif

   use :: forgex_automaton_m, only: automaton_t
   use :: forgex_api_internal_m, only: do_matching_exactly
   implicit none
   private

   ! public :: operator(.in.)
   public :: operator(.match.)
   ! public :: regex

   ! interface operator(.in.)
   !    !! Interface for user-defined operator of `.in.`
   !    module procedure :: in__matching
   ! end interface 

   interface operator(.match.)
   !! Interface for user-defined operator of `.match.`
      module procedure :: operator__match
   end interface 

   ! interface regex
   !    !! The generic name for the `regex` function implemented as `regex__matching`.
   !    module procedure :: subroutine__regex
   ! end interface


contains

   function operator__match(pattern, str) result(res)
      !! The function implemented for the `.match.` operator.
      implicit none
      character(*), intent(in)       :: pattern, str
      logical                        :: res

      character(:),      allocatable :: buff
      type(tree_node_t), allocatable :: tree(:)
      type(tape_t)                   :: tape
      type(automaton_t)              :: automaton
      integer                        :: root 


      buff = trim(pattern)

      ! Build a syntax tree from buff, and store the result in tree and root.
      call build_syntax_tree(buff, tape, tree, root)

#ifdef IMPURE
#ifdef DEBUG
      call print_tree(tree, root)
#endif
#endif

      ! Initialize automaton with tree and root.
      call automaton%init(tree, root)

      ! Call the internal procedure to match string, and store the result in logical `res`. 
      call do_matching_exactly(automaton, str, res)

#ifdef IMPURE
#ifdef DEBUG
      call automaton%print_dfa()
#endif
#endif

   end function operator__match


!---------------------------------------------------------------------!
! Private procedures
!

   !> This function returns .true. if the pattern contains the caret character
   !> at the top that matches the beginning of a line.
   function is_there_caret_at_the_top(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff
      logical :: res

      buff = adjustl(pattern)
      if (len(buff) == 0) return

      res = buff(1:1) == '^'
   end function is_there_caret_at_the_top


   !> This funciton returns .true. if the pattern contains the doller character
   !> at the end that matches the ending of a line.
   function is_there_dollar_at_the_end(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff

      logical :: res

      buff = trim(pattern)
      if (len(buff) == 0) return

      res = buff(len_trim(buff):len_trim(buff)) == '$'
   end function is_there_dollar_at_the_end


end module forgex