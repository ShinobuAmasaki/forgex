program test_case_015
   use :: iso_fortran_env, only: error_unit
   use :: forgex_test_m 
   implicit none
   logical :: res

   res = .true.
   print *, '=== TEST CASE 15 BEGIN ==='

   call runner_match("abc\d", "abc", .false., res)
   call runner_match("abc\d", "abc1", .true., res)
   call runner_match("abc\d+", "abc1", .true., res)
   call runner_match("abc[0-9]", "abc1", .true., res)
   call runner_match("abc\w", "abcd", .true., res)
   call runner_match("abc\w", "abc", .false., res)

   if (res) then
      write(error_unit, *) '=== TEST CASE 15 END ==='
      stop
   else
      error stop
   end if
end program test_case_015