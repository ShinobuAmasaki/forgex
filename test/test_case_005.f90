program test_005
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== TEST CASE 5 BEGIN ==="

   call runner_match("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .true., res)

   call runner_match("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaakaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .false., res)

   call runner_in("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaakaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .true., res)
  
   call runner_match("((a|b|c|d|e|f|g|h|i|j){1,5}){,3}", "abcdefghij", .true., res)

   call runner_match("aa*b", "cb", .false., res)
   call runner_match("a(a|aa)*b", "acb", .false., res)


   if (res) then
      print *, "=== TEST CASE 5 END ==="
      stop
   else
      error stop
   end if
end program test_005