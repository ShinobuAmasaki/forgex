program ast_case_003
   use :: forgex_test_m
   implicit none
   logical :: res = .true.

   print *, "=== AST CASE 3 BEGIN ==="
   call runner_middle("[ab]*a[ab]{20}", "a", res)
   call runner_middle("[a]{3,4}", "aaa", res)
   call runner_middle("(a|b)+", "", res)
   call runner_middle("(b?)a*(cd){3,4}", "cdcdcd", res)
   call runner_middle("(b+)a*(cd){3,4}", "cdcdcd", res)
   call runner_middle("(b?)a+(cd){3,4}", "a", res)
   call runner_middle("z(ab|ac|a+){3,4}", "", res)
   call runner_middle("z(ab|ac){3,4}", "", res)
   call runner_middle("ab*c+d*e", "c", res)
   call runner_middle("(ab){3,4}", "ababab", res)
   call runner_middle("c{5,10}", "ccccc", res)
   call runner_middle("c{4,10}", "cccc", res)
   call runner_middle("c{4,7}", "cccc", res)
   call runner_middle("c{2,3}", "cc", res)
   call runner_middle("c{1,3}", "c", res)
   call runner_middle("c{0,3}", "", res)

   call runner_middle("ab*c+d", "c", res)
   call runner_middle("a(cd)+e", "cd", res)
   call runner_middle("x(ab|cd)y", "y", res)
   call runner_middle("a(bc|de)f", "f", res)
   call runner_middle("ab*(cd)+ef", "cd", res)
   call runner_middle("(ab|cd)efg", "efg", res)
   call runner_middle("(abc|def)gh", "gh", res)
   call runner_middle("a(bc|de)fg", "fg", res)
   call runner_middle("x(yz|ab)cd", "cd", res)
   call runner_middle("a(b(cd)+)e", "cd", res)
   call runner_middle("x(y*z(cd)+ef)g", "zcd", res)
   call runner_middle("a(bc|de)(fg)+h", "fg", res)
   call runner_middle("x(y|z)*ab", "ab", res)
   call runner_middle("a(bc)+d", "bc", res)
   call runner_middle("(ab)*c(de)+f", "cde", res)


   if (res) then
      print *, "=== AST CASE 3 END ==="
      stop
   else
      error stop
   end if

end program ast_case_003
