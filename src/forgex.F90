#ifdef PURE
#define pure
#endif
module forgex
   use :: forgex_syntax_tree_m
   use :: forgex_automaton_m
   use :: forgex_api_internal_m
   implicit none
   private

   ! public :: operator(.in.)
   public :: operator(.match.)
   ! public :: regex

   interface operator(.match.)
      module procedure :: operator__match
   end interface

contains

    function operator__match(pattern, str) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: pattern, str
      logical :: res

      character(:), allocatable :: buff
      
      type(tree_node_t), allocatable :: tree(:)
      integer :: root 
      type(tape_t) :: tape

      type(automaton_t) :: automaton

      buff = pattern

      call build_syntax_tree(buff, tape, tree, root)

#ifdef PURE 
      call print_tree(tree, root)
#endif

      call automaton%init(tree, root)

      call do_matching_exactly(automaton, str, res)

#ifdef PURE
      call automaton%print_dfa()
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