!> This case covers shorthands in character classes.
program main
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   print *, "=== PATTERN VALIDATE CASE 3 BEGIN ==="
!=====================================================================!

   call runner_validate("[\t]", .true., res)
   call runner_validate("[\n]", .true., res)
   call runner_validate("[\r]", .true., res)
   call runner_validate("[\s]", .true., res)
   call runner_validate("[\S]", .true., res)
   call runner_validate("[\w]", .true., res)
   call runner_validate("[\W]", .true., res)
   call runner_validate("[\d]", .true., res)
   call runner_validate("[\D]", .true., res)

   call runner_validate("[\t\d]", .true., res)
   call runner_validate("[^\t]", .true., res)
   call runner_validate("[\a]", .false., res)
   call runner_validate("[\あ]", .false., res)
   call runner_validate("[\r-a]", .true., res)

   ! Below results will be changed after implementing character class subtraction.
   call runner_validate("[a-z--b]", .false., res)
   call runner_validate("[a-z--b-z]", .false., res)
   call runner_validate("[a--a]", .false., res)

!=====================================================================!
   if (res) then
      print *, "=== PATTRERN VALIDATE CASE 3 END ==="
      stop
   else
      error stop
   end if

end program main