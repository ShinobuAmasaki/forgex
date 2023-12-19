module in_operator_m
   use, intrinsic :: iso_fortran_env
   use :: syntax_tree_m
   use :: automaton_m
   implicit none

   public :: operator(.in.)
   
   interface operator(.in.)
      module procedure :: in__matching
   end interface

   type(automaton_t) :: cache

contains

   function in__matching (pattern, str) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32) :: from, to
      logical :: res

      type(tree_t), pointer :: root

      from = 0
      to = 0

      root => parse_regex(pattern)

      call cache%init()
      call cache%build_nfa(root)
      call cache%convert_NFA_to_DFA()
      call cache%matching(str, from, to)

      if (from  > 0 .and. to > 0) then
         res = .true.
      else
         res = .false.
      end if

      ! call cache%deallocate_automaton()

   end function in__matching

end module in_operator_m