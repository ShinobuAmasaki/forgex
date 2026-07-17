! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_sort_m module is a part of Forgex.
!
!! This file contains sorting algorithm implementations.

!> The `forgex_sort_m` module provides an implementation of
!> sorting algorithms for integer arrays.
module forgex_sort_m
   use, intrinsic :: iso_fortran_env
   implicit none

   !| Currently, complex sorting algorithms are not required, only simple algorithms
   !  are used, but this does not constrain future implementations.

contains

   pure subroutine insertion_sort(list)
      implicit none
      integer, intent(inout) :: list(:)

      integer :: i, j, key

      do i = 2, size(list, dim=1)
         key = list(i)
         j = i - 1

         do while(j > 0 .and. list(j) > key)
            list(j+1) = list(j)
            j = j - 1
            if (j == 0) exit
         end do
         list(j + 1) = key
      end do
   end subroutine insertion_sort

end module forgex_sort_m