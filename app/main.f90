program main
   use :: forgex
   use :: forgex_time_measurement_omp_m ! exec: fpm run --flag "-DOMP -fopenmp"
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