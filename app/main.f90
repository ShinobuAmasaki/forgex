program main
   use :: forgex
   use :: forgex_time_measurement_m
   implicit none
   
   logical :: res, entire

   entire = .true.
   call time_begin()
   
   do i = 1, 1000
      res = '\d{3}-\d{3}-\d{3}-\d{3}-\d{3}-\d{3}' .in. '123-456-789-123-456-789'
      entire = entire .and. res
   end do

   call time_lap("")
   call time_end("1000 loops .in. operator")

   print *, entire 
   
end program main