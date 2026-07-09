program test_case_015
   use :: iso_fortran_env, only: error_unit
   use :: forgex_test_m 
   implicit none
   logical :: res

   res = .true.
   print *, '=== TEST CASE 15 BEGIN ==='

   ! fixed on the 329a00b0 commit
   call runner_match("abc\d", "abc", .false., res)
   call runner_match("abc\d", "abc1", .true., res)
   call runner_match("abc\d+", "abc1", .true., res)
   call runner_match("abc[0-9]", "abc1", .true., res)
   call runner_match("abc\w", "abcd", .true., res)
   call runner_match("abc\w", "abc", .false., res)
   
   ! fixed on the 7a3c2d08 commit 
   call runner_in(".", ' ', .true., res)
   call runner_in("\s", ' ', .true., res)
   call runner_in("\S", ' ', .false., res)
   call runner_match("\s", ' ', .true., res)
   call runner_match(".", ' ', .true., res)

   if (res) then
      write(error_unit, *) '=== TEST CASE 15 END ==='
      stop
   else
      error stop
   end if
end program test_case_015