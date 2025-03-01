!> These test cases cover invalid regex patterns.
program main
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   print *, "=== PATTERN VALIDATE CASE 2 BEGIN ==="
!=====================================================================!

   call runner_validate("abc\[]", .false., res)
   call runner_validate("abc(", .false., res)
   call runner_validate("abc[333", .false., res)
   call runner_validate("abc]333", .false., res)
   
   call runner_validate("[", .false., res)
   call runner_validate("]", .false., res)
   call runner_validate("a[", .false., res)
   call runner_validate("a]", .false., res)
   call runner_validate("]a", .false., res)
   call runner_validate("[a", .false., res)
   call runner_validate("a[b", .false., res)
   call runner_validate("a]b", .false., res)
   
   call runner_validate("(", .false., res)
   call runner_validate(")", .false., res)
   call runner_validate("a(", .false., res)
   call runner_validate("a)", .false., res)
   call runner_validate("(a", .false., res)
   call runner_validate(")a", .false., res)
   call runner_validate("a(b", .false., res)
   call runner_validate("a)b", .false., res)
   

   call runner_validate("{", .false., res)
   call runner_validate("}", .true., res)
   call runner_validate("a{", .false., res)
   call runner_validate("a}", .true., res)
   call runner_validate("{a", .false., res)
   call runner_validate("}a", .true., res)
   call runner_validate("a{b", .false., res)
   call runner_validate("a}b", .true., res)

   call runner_validate("a{1,2", .false., res)
   call runner_validate("a1,b}", .true., res)
   call runner_validate("a{b,1}", .false., res)
   call runner_validate("a{1,b}", .false., res)
   call runner_validate("a{2,1}", .false., res)

   call runner_validate("[]", .false., res)
   call runner_validate("[-]", .true., res)
   call runner_validate("[--]", .false., res)
   call runner_validate("[---]", .false., res)
   call runner_validate("[-a--]", .false., res)
   call runner_validate("[-a--b]", .false., res)

   call runner_validate("[{]", .true., res)
   call runner_validate("[}]", .true., res)

   call runner_validate("[+]", .true., res)
   call runner_validate("[\[-\]]", .true., res)
   call runner_validate("[\[-\\]", .true., res)
   call runner_validate("[a]{3,4}", .true., res)
   call runner_validate("[--]", .false., res)
   call runner_validate("[+--]", .false., res)
   call runner_validate("\\\", .false., res)
   call runner_validate("\\", .true., res)
   call runner_validate("\", .false., res)
   call runner_validate("[^]", .false., res)
   call runner_validate("[\{]", .true., res)

   call runner_validate("[\\]", .true., res)
   call runner_validate("[\\\]]", .true., res)
   call runner_validate("[\\\]]", .true., res)

   call runner_validate("[a-z]", .true., res)
   call runner_validate("[\a]", .false., res)
   call runner_validate("[z-a]", .false., res)
   call runner_validate("[a-]", .false., res)
   call runner_validate("[\{-]", .false., res)
   
   call runner_validate("(a*)*", .true., res)

   call runner_validate("(\w+\s*)+", .true., res)
   call runner_validate("\{k\}", .true., res)
   call runner_validate("\{k}", .true., res)
   call runner_validate("}", .true., res)
   
!=====================================================================!
   if (res) then
      print *, "=== PATTERN VALIDATE CASE 2 END ==="
      stop
   else
      error stop
   end if

end program main
   