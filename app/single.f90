program main
   use :: forgex
   use :: forgex_time_measurement_m
   implicit none
   
   logical :: res, entire

   call time_begin()
   res = '\d{3}-\d{3}-\d{3}-\d{3}-\d{3}-\d{3}' .in. '123-456-789-123-456-789'
   call time_end(".in. operator")
   print *, res
   
end program main