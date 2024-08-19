program test_006
   use, intrinsic :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   ! Test case #6.
   ! Tests for shorthand of character classes.

   print *, "=== TEST CASE 6 BEGIN ==="

   call runner_match("\s", ' ', .true., res)       ! == char(32)
   call runner_match("\s", char(9), .true., res)
   call runner_match("\s", char(10), .true., res)
   call runner_match("\s", char(12), .true., res)
   call runner_match("\s", char(13), .true., res)
   call runner_match("\s", "　", .true., res)


   call runner_match("\S", "!", .true., res)       ! == char(33)
   call runner_match("\S", char(31), .true., res)
   call runner_match("\S", " ", .false., res)
   call runner_match("\S", "a", .true., res)
   call runner_match("\S", "A", .true., res)
   call runner_match("\S", "x", .true., res)
   call runner_match("\S", "Z", .true., res)


   call runner_match("\d{3}", "012", .true., res)
   call runner_match("\d{3}", "234", .true., res)
   call runner_match("\d{3}", "089", .true., res)
   call runner_match("\d{3}", "/9:", .false., res)

   call runner_match("\D", "a", .true., res)
   call runner_match("\D", "/", .true., res)
   call runner_match("\D", ":", .true., res)
   call runner_match("\D", "A", .true., res)
   call runner_match("\D", "0", .false., res)
   call runner_match("\D", "1", .false., res)
   call runner_match("\D", "2", .false., res)
   call runner_match("\D", "3", .false., res)
   call runner_match("\D", "4", .false., res)
   call runner_match("\D", "5", .false., res)
   call runner_match("\D", "6", .false., res)
   call runner_match("\D", "7", .false., res)
   call runner_match("\D", "8", .false., res)
   call runner_match("\D", "9", .false., res)

   call runner_match("\n", char(10), .true., res)  ! LF
   call runner_match("\n", char(13)//char(10), .true., res) ! CRLF

   call runner_match("\r", char(13), .true., res)  ! CR

   call runner_match("\t", char(9), .true., res)

   call runner_match("\w", "A", .true., res)
   call runner_match("\w", "B", .true., res)
   call runner_match("\w", "Z", .true., res)
   call runner_match("\w", "a", .true., res)
   call runner_match("\w", "b", .true., res)
   call runner_match("\w", "z", .true., res)
   call runner_match("\w", "_", .true., res)
   call runner_match("\w", "0", .true., res)
   call runner_match("\w", "9", .true., res)
   call runner_match("\w", "/", .false., res)
   call runner_match("\w", " ", .false., res)
   call runner_match("\w", ":", .false., res)
   call runner_match("\w", "@", .false., res)
   call runner_match("\w", "[", .false., res)
   call runner_match("\w", "`", .false., res)
   call runner_match("\w", "{", .false., res)
   call runner_match("\w", "^", .false., res)
   call runner_match("\w", "`", .false., res)


   call runner_match("\W", "A", .false., res)
   call runner_match("\W", "B", .false., res)
   call runner_match("\W", "Z", .false., res)
   call runner_match("\W", "a", .false., res)
   call runner_match("\W", "b", .false., res)
   call runner_match("\W", "z", .false., res)
   call runner_match("\W", "_", .false., res)
   call runner_match("\W", "0", .false., res)
   call runner_match("\W", "9", .false., res)
   call runner_match("\W", "/", .true., res)
   call runner_match("\W", " ", .true., res)
   call runner_match("\W", ":", .true., res)
   call runner_match("\W", "@", .true., res)
   call runner_match("\W", "[", .true., res)
   call runner_match("\W", "`", .true., res)
   call runner_match("\W", "{", .true., res)


   if (res) then
      print *, "=== TEST CASE 6 END ==="
      stop
   else
      error stop
   end if


end program test_006