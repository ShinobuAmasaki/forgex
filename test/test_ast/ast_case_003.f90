program ast_case_003
   use :: forgex_test_m
   implicit none
   logical :: res = .true.

   print *, "=== AST CASE 3 BEGIN ==="
   call runner_middle("[ab]*a[ab]{20}", "a", res)
   call runner_middle("[a]{3,4}", "aaa", res)
   call runner_middle("(a|b)+", "", res)
   call runner_middle("(b?)a*(cd){3,4}", "", res)
   call runner_middle("(b+)a*(cd){3,4}", "", res)
   call runner_middle("(b?)a+(cd){3,4}", "a", res)
   call runner_middle("z(ab|ac|a+){3,4}", "", res)
   call runner_middle("z(ab|ac){3,4}", "", res)
   call runner_middle("(ab){3,4}", "ababab", res)
   call runner_middle("c{5,10}", "ccccc", res)
   call runner_middle("c{4,10}", "cccc", res)
   call runner_middle("c{4,7}", "cccc", res)
   call runner_middle("c{2,3}", "cc", res)
   call runner_middle("c{1,3}", "c", res)
   call runner_middle("c{0,3}", "", res)


   if (res) then
      print *, "=== AST CASE 3 END ==="
      stop
   else
      error stop
   end if

end program ast_case_003
