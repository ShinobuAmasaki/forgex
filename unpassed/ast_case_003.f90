program main
   use :: iso_fortran_env
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== AST CASE 2 BEGIN ==="

!========================================================================================!
   ! v4.4 is not able to get the longest suffix.
   call runner_suffix("[a]{3,4}", "aaa", res)
   call runner_suffix("(b+)a*(ab){3,4}", "ababab", res)
   call runner_suffix("c{5,10}", "ccccc", res)
   call runner_suffix("c{4,10}", "cccc", res)
   call runner_suffix("c{4,7}", "cccc", res)
   call runner_suffix("c{2,3}", "cc", res)
   call runner_suffix("c{1,3}", "c", res)
   call runner_suffix("((ab)+){2}", "ab", res) !! abab is expected.
   call runner_suffix("(ab){3,4}", "ababab", res)

   call runner_suffix("a(bbc|aac|abc)*b{3}", "bbb", res)
   call runner_suffix("(ad{2,3}|cd{1,2}){3}", "d", res)
   
   call runner_suffix("((ab)?c+){2,3}", "c", res)

   call runner_suffix("a((ac|bc)+){3}", "c", res)

   call runner_suffix('(a*c){2}', 'c', res)  ! Failed in v4.3
   call runner_suffix('(a*c){3}', 'c', res)  ! Failed in v4.3
   call runner_suffix('((a*c){3}d)', 'cd', res)  ! Failed in v4.3

   call runner_suffix('((a*c){3}d){2}', 'cd', res)  ! Failed in v4.3
   call runner_suffix('((a*c){3}d){2}', 'cd', res)  ! Failed in v4.3
   call runner_suffix('((a*c){3}d){2}', 'cd', res)  ! Failed in v4.3
   
   call runner_suffix('((a*c){3}*d){2}b', 'db', res)  ! Failed in v4.3
   call runner_suffix('((a*c){3}*d){2}b', 'db', res)  ! Failed in v4.3
   call runner_suffix('((a*c){3}*d){2}b', 'db', res)  ! Failed in v4.3

   call runner_suffix("(aa|ba){3}b", "ab", res)
   call runner_suffix("a(ac|bc|cc){3}", "c", res)

   !! If the + operator is not processed on op_closure handling in get_suffix_literal_internal,
   !! no suffix is ​​obtained.
   ! call runner_suffix("(ab|bb|cb)+", "", res)
   ! call runner_suffix("((ab)+){2}", "", res)
   ! call runner_suffix("((ab|bb)|(db|eb))+", "", res)
   ! call runner_suffix("((ac|bc)+|(dc|ec)+)+", "", res)
   ! call runner_suffix("((ab)?c+){2,3}", "", res)
   ! call runner_suffix("((a|b)+c)+", "", res)

   if (res) then
      write(error_unit, *) "=== AST CASE 2 END ==="
      stop
   else
      error stop
   end if

end program main