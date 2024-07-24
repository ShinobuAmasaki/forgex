module forgex

   implicit none
   private

   public :: operator(.in.)
   ! public :: operator(.match.)
   ! public :: regex

   interface operator(.in.)
      module procedure :: operator__in_matching
   end interface

contains

   pure function operator__in_matching(pattern, str) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: pattern, str
      logical :: res

      character(:), allocatable :: buff
      integer(int32) :: from, to
      
   end function 

end module forgex