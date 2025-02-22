program test_case_008
   use ::forgex_test_m
   implicit none

   logical :: res = .true.

   ! Corner cases
   print *, "=== TEST CASE 7 BEGIN ==="

   call runner_match("[]", "", .false., res)
   call runner_match("[-]", "-", .true., res)
   call runner_match("[+]", "+", .true., res)

   call runner_match("[\{]", "{", .true. , res)
   call runner_match("[\{]+", "{{", .true. , res)

   call runner_match("[\[]", "[", .true. , res)
   call runner_match("[\[]+", "[[", .true. , res)
   call runner_match("[\[-\]]", "[", .true. , res)
   call runner_match("[\[-\]]", "\", .true. , res)
   call runner_match("[\]]", "]", .true. , res)

   call runner_match("[\{-\}]", "{", .true. , res)
   call runner_match("[\{-\}]", "|", .true. , res)
   call runner_match("[-\{-\}]*", "{|}", .true. , res)
   call runner_match("[\}]", "}", .true. , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)
   ! call runner_match("", "", , res)


   if (res) then
      print *, "=== TEST CASE 7 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if
end program test_case_008