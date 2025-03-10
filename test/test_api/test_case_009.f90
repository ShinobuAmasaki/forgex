program test_case_008
   use :: iso_fortran_env
   use :: forgex_utf8_m
   use :: forgex_test_m
   implicit none

   logical :: res = .true.
   integer :: i
   character(:), allocatable :: c

   ! Shorthands within character set.
   print *, "=== TEST CASE 9 BEGIN ==="
!=====================================================================!

   call runner_match("[\t]", char(9), .true., res)
   call runner_match("[\n]", char(10), .true., res)
   call runner_match("[\r]", char(10), .false., res)
   call runner_match("[\n]", char(13), .true., res)
   call runner_match("[\r]", char(13), .true., res)
   call runner_match("[\n]+", char(13)//char(10), .true., res)

   call runner_match("[\w]", "a", .true., res)
   call runner_match("[\w]", "z", .true., res)
   call runner_match("[\w]", "A", .true., res)
   call runner_match("[\w]", "Z", .true., res)
   call runner_match("[\w]", "_", .true., res)
   call runner_match("[\w]", "0", .true., res)
   call runner_match("[\w]", "9", .true., res)

   call runner_match("[\W]", "a", .false., res)
   call runner_match("[\W]", "z", .false., res)
   call runner_match("[\W]", "A", .false., res)
   call runner_match("[\W]", "Z", .false., res)
   call runner_match("[\W]", "_", .false., res)
   call runner_match("[\W]", "0", .false., res)
   call runner_match("[\W]", "9", .false., res)
   call runner_match("[\W]", "/", .true., res)
   call runner_match("[\W]", ":", .true., res)
   call runner_match("[\W]", "@", .true., res)
   call runner_match("[\W]", "[", .true., res)
   call runner_match("[\W]", "`", .true., res)
   call runner_match("[\W]", "~", .true., res)

   call runner_match("[\t\d]", char(9), .true., res)
   call runner_match("[\t\d]", "0", .true., res)

   do i = 13, ichar("a")
      call runner_match("[\r-a]", char(i), .true., res)
   end do

   call runner_match("[a\d]", "a", .true., res)
   call runner_match("[a\d]", "0", .true., res)
   call runner_match("[a\d]", "9", .true., res)
   call runner_match("[a\d]", "b", .false., res)

   call runner_match("a b", "a b", .true., res)
   call runner_match("a* bc*", "a bc", .true., res)
   call runner_match("a* bc*", "a bc", .true., res)
   call runner_in("a* bc*", " b", .true., res)

!=====================================================================!
   if (res) then
      write(error_unit, *) "=== TEST CASE 9 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if
end program test_case_008