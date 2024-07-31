program main
   use :: forgex
   use :: forgex_time_measurement_m
   implicit none
   
   call time_begin
   print *, '\d{3}-\d{3}-\d{3}-\d{3}-\d{3}-\d{3}' .in. '123-456-789-123-456-789'
   call time_lap(".in. operator")
end program main