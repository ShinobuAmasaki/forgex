program test_004
   use, intrinsic :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   logical  :: res = .true.

   print *, "=== TEST CASE 4 BEGIN ==="

   call runner_match("==", "==", .true., res)

   call runner_match("/=", "/=", .true., res)

   call runner_match("<=", "<=", .true., res)

   call runner_match("(==|/=|<=|>=)", "==", .true., res)

   call runner_match("[-+*/()<>]", "+", .true., res)
   call runner_match("[-+*/()<>]", "-", .true., res)
   call runner_match("[-+*/()<>]", "*", .true., res)
   call runner_match("[-+*/()<>]", "/", .true., res)
   call runner_match("[-+*/()<>]", "(", .true., res)
   call runner_match("[-+*/()<>]", ")", .true., res)
   call runner_match("[-+*/()<>]", "<", .true., res)
   call runner_match("[-+*/()<>]", ">", .true., res)
   call runner_match("\.", ".", .true., res)

   if (res) then
      print *, "=== TEST CASE 4 END ==="
      stop
   else
      error stop
   end if

end program test_004