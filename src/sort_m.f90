!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023-2024
!!     A regular expression engine for Fortran.
!!     forgex_sort_m module is a part of Forgex.
!!
module forgex_sort_m
   use, intrinsic :: iso_fortran_env
   implicit none
   
contains

   subroutine bubble_sort(list)
      implicit none
      integer(int32), intent(inout) :: list(:)

      integer :: i, j, siz, tmp

      siz = size(list)

      do i = 1, siz-1
         do j = i+1, siz
            if (list(i) > list(j)) then
               tmp = list(i)
               list(i) = list(j)
               list(j) = tmp
            end if
         end do
      end do

   end subroutine bubble_sort

end module forgex_sort_m