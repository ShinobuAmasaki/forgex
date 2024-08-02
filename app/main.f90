program main
   use :: forgex
   use :: forgex_time_measurement_cpu_m
   implicit none

   logical :: res, entire
   integer :: i

   entire = .true.
   call time_begin()

   do i = 1, 1000
      res = 'a[^x]{20}b' .in. 'aaakkkkkkkkkkkkkkkkkkkb'
      entire = entire .and. res
      if (mod(i, 100) == 0) print *, "i = ", i, res
   end do

   call time_lap("")
   call time_end("1000 loops .in. operator")

   print *, entire

end program main