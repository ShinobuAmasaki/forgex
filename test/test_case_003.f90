program test_003
   use :: test_m
   implicit none

   ! Test case #3. 
   ! Tests for digit Character set.


   call runner_match('\d{3}-\d{2}-\d{4}', "123-45-6789", .true.)
   call runner_match('\d{3}-\d{2}-\d{4}', "123456789", .false.)
   call runner_match('[0-9]{10}', '0987654321', .true.)
   call runner_match('\d{3}', '01', .false.)

   call runner_match('\d{3}\?', '001?', .true.)

   call runner_match("\D", 'a', .true.)

   call runner_match("[^0-9]?", "a", .true.)
   call runner_match("[^0-9]{0}", "", .true.)
   call runner_match("[^0-9]{1}", "a", .true.)
   call runner_match("[^0-9]{2}", "ab", .true.)
   call runner_match("[^0-9]{3}", "abc", .true.)

   call runner_match("\d{3}-\d{4}", "100-1002", .true.)
   call runner_match("\d{3}-\d{4}", "1234567", .false.)


   print *, "=== TEST CASE 3 END ==="
end program test_003