program test_case_008
   use :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   ! Corner cases
   print *, "=== TEST CASE 8 BEGIN ==="

   call runner_match("[]", "", .false., res)
   call runner_match("[-]", "-", .true., res)
   call runner_match("[+]", "+", .true., res)

   call runner_match("[\{]", "{", .true. , res)
   call runner_match("[\{]+", "{{", .true. , res)

   call runner_match("[\[]", "[", .true. , res)
   call runner_match("[\[]+", "[[", .true. , res)
   call runner_match("[\\]", "\", .true., res)
   call runner_match("[\\\]]", "]", .true., res)
   call runner_match("[\\\]]", "\", .true., res)
   call runner_match("[\[-\]]", "[", .true. , res)
   call runner_match("[\[-\]]", "\", .true. , res)
   call runner_match("[\]]", "]", .true. , res)

   call runner_match("[\{-\}]", "{", .true. , res)
   call runner_match("[\{-\}]", "|", .true. , res)
   call runner_match("[-\{-\}]*", "{|}", .true. , res)
   call runner_match("[\}]", "}", .true. , res)

   call runner_match("(a*)*", "aaa", .true., res)
   call runner_match("(.+)*", "xyz", .true., res)
   call runner_match("(\d+)?", "", .true., res)
   call runner_match("(\d{2,4}-\d{2,4}-\d{2,4})", "1234-567-890", .true., res)
   call runner_match("(\d{2,4}-\d{2,4}-\d{2,4})", "1234--567", .false., res)
   call runner_match("(\w+\s*)+", "Hi Alice", .true., res)
   call runner_match("(\w+\s*)+", "123456", .true., res)
   call runner_match("(\w+\s*)+", "123456 foo", .true., res)

   call runner_match("a{0}", "", .true., res)

   call runner_match("\\*", "\", .true., res)
   call runner_match("\\*", "\\", .true., res)
   call runner_match("\\*", "\\\", .true., res)
   call runner_match("\\?", "\", .true., res)
   call runner_match("\\?", "\\", .false., res)
   call runner_match("\\+", "", .false., res)
   call runner_match("\\+", "\", .true., res)
   call runner_match("\\+", "\\", .true., res)
   call runner_match("\\+", "\\\", .true., res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)


   if (res) then
      write(error_unit, *)"=== TEST CASE 8 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if
end program test_case_008