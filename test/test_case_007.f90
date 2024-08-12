program test_case_007
   use ::forgex_test_m
   implicit none

   logical :: res = .true.

   print *, "=== TEST CASE 7 BEGIN ==="
   call runner_match("(a|b)*", 'ababab', .true., res)
   call runner_match("(a|b)+", 'ababab', .true., res)
   call runner_match("(a|b)?", 'a', .true., res)

   call runner_match("((a|b)*)*", 'ababab', .true., res)
   call runner_match("((a|b)*)?", 'ababab', .true., res)
   call runner_match("((a|b)*)+", 'ababab', .true., res)
   
   if (res) then
      print *, "=== TEST CASE 7 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if

   
end program test_case_007