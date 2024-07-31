program test_005
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== TEST CASE 5 BEGIN ==="

   call runner_match("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .true., res)

   if (res) then
      print *, "=== TEST CASE 5 END"
      stop
   else
      error stop
   end if
end program test_005