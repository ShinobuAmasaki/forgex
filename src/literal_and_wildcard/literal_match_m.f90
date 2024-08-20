module forgex_literal_match_m
   use :: iso_fortran_env, only: int32
   implicit none
   private

   public :: literal_index_matching

   type, public :: from_to_result_t
      integer(int32) :: from = 0
      integer(int32) :: to = 0
      character(:), allocatable :: substr
   end type from_to_result_t
   
contains

   pure subroutine literal_index_matching(pattern, text, from, to)
      implicit none
      character(*), intent(in) :: pattern, text
      integer(int32), intent(inout) :: from, to

      from = index(text, pattern)
      to = from + len(pattern) -1

   end subroutine literal_index_matching

   pure subroutine literal_kmp_search(pattern, text, array)
      implicit none
      character(*), intent(in) :: pattern
      character(*), intent(in) :: text
      type(from_to_result_t), intent(inout), allocatable :: array(:)
      
   end subroutine literal_kmp_search

end module forgex_literal_match_m