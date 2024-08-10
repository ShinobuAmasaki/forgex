module forgex_cli_utils_m
   implicit none
   
contains

   subroutine right_justify(array)
      use :: forgex_cli_parameters_m
      implicit none
      character(NUM_DIGIT_KEY), intent(inout) :: array(:)

      character(NUM_DIGIT_KEY), allocatable :: buff(:)
      integer :: i, max_len


      allocate(buff(size(array, dim=1)))
      buff(:) = array(:)

      max_len = 0
      do i = 1, size(buff)
         max_len = max(max_len, len_trim(adjustl(buff(i))))
      end do

      ! right justify
      do i = 1, size(buff)
         buff(i) = adjustl(array(i))
         buff(i) = repeat(' ', max_len- len_trim(buff(i)))// buff(i)
      end do

      array(:) = buff(:)

   end subroutine

end module forgex_cli_utils_m