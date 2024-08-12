program test_case_007
   use ::forgex_test_m
   implicit none

   logical :: res = .true.

   ! Nested cases

   print *, "=== TEST CASE 7 BEGIN ==="
   call runner_match("(a|b)*", 'ababab', .true., res)
   call runner_match("(a|b)+", 'ababab', .true., res)
   call runner_match("(a|b)?", 'a', .true., res)

   call runner_match("((a|b)*)*", 'ababab', .true., res)
   call runner_match("((a|b)*){0,}", 'ababab', .true., res)
   call runner_match("((a|b)*)?", 'ababab', .true., res)
   call runner_match("((a|b)*)+", 'ababab', .true., res)

   call runner_match("(a*)*", "", .true., res)
   call runner_match("(a*)*", "a", .true., res)
   call runner_match("(a*)*", "aaa", .true., res)
   call runner_match("(a*)*", "aaaaa", .true., res)

   call runner_match("a?b+|c*d", "bbbb", .true., res)
   call runner_match("a?b+|c*d", "a", .false., res)
   call runner_match("a?b+|c*d", "b", .true., res)
   call runner_match("a?b+|c*d", "cd", .true., res)
   call runner_match("a?b+|c*d", "d", .true., res)
   call runner_match("a?b+|c*d", "a", .false., res)
   call runner_in("a?b+|c*d", "bbd", .true., res)

   
   if (res) then
      print *, "=== TEST CASE 7 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if

   
end program test_case_007