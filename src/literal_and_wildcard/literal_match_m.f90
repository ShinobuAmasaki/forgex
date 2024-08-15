module forgex_literal_match_m
   use :: iso_fortran_env, only: int32
   implicit none
   
contains

   pure subroutine literal_index_matching(pattern, text, from, to)
      implicit none
      character(*), intent(in) :: pattern, text
      integer(int32), intent(inout) :: from, to

      from = index(text, pattern)
      to = from + len(pattern) -1

   end subroutine literal_index_matching

end module forgex_literal_match_m