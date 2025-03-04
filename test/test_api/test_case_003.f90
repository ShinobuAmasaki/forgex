program test_003
   use :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   ! Test case #3.
   ! Tests for digit Character set.

   logical :: res = .true.

   print *, "=== TEST CASE 3 BEGIN ==="

   call runner_match('\d{3}-\d{2}-\d{4}', "123-45-6789", .true.,res)
   call runner_match('\d{3}-\d{2}-\d{4}', "123456789", .false., res)
   call runner_match('[0-9]{10}', '0987654321', .true., res)
   call runner_match('\d{3}', '01', .false., res)

   call runner_match('\d{3}\?', '001?', .true., res)

   call runner_match("\D", 'a', .true., res)

   call runner_match("[^0-9]?", "a", .true., res)
   call runner_match("[^0-9]{1}", "a", .true., res)
   call runner_match("[^0-9]{2}", "ab", .true., res)
   call runner_match("[^0-9]{3}", "abc", .true., res)

   call runner_match("\d{3}-\d{4}", "100-1002", .true., res)
   call runner_match("\d{3}-\d{4}", "1234567", .false., res)


   if (res) then
      write(error_unit, *) "=== TEST CASE 3 END ==="
      stop
   else
      error stop
   end if

end program test_003