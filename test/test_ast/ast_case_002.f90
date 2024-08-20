program main
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== AST CASE 2 BEGIN ==="

   call runner_postfix("[a]{3,4}", "aaa", res)
   call runner_postfix("(b+)a*(ab){3,4}", "ababab", res)
   call runner_postfix("(ab|ac|a+){3,4}", "", res)
   call runner_postfix("(ab|ac){3,4}", "", res)
   call runner_postfix("(ab){3,4}", "ababab", res)
   call runner_postfix("c{5,10}", "ccccc", res)
   call runner_postfix("c{4,10}", "cccc", res)
   call runner_postfix("c{4,7}", "cccc", res)
   call runner_postfix("c{2,3}", "cc", res)
   call runner_postfix("c{1,3}", "c", res)
   call runner_postfix("c{0,3}", "", res)

   call runner_postfix("(aa|ab|ac)", "", res)
   call runner_postfix("(ああう|あいう|あうう)", "う", res)
   call runner_postfix("(あああ|いああ|うああ)", "ああ", res)
   call runner_postfix("あ", "あ", res)
   call runner_postfix("a.{1,5}g", "g", res)
   call runner_postfix("(ab|aa){3}b", "b", res)
   call runner_postfix("a(ac|bc|cc){3}", "c", res)
   call runner_postfix("a(bbc|aac|abc)*", "", res)
   call runner_postfix("a(bbc|aac|abc)*b{3}", "bbb", res)
   call runner_postfix("(ad{2,3}|cd{1,2}){3}", "d", res)
   call runner_postfix("(ab?|cd?){2,3}", "", res)
   call runner_postfix("(xyz|ab|abc|ac){2,4}", "", res)
   call runner_postfix("\d{3,5}a\sb", "b", res)
   call runner_postfix("((ab)?c?){2,3}", "", res)
   call runner_postfix("((ab)?c+){2,3}", "c", res)
   call runner_postfix("(ab)+", "ab", res)
   call runner_postfix("(ab|bb|cb)+", "b", res)
   call runner_postfix("((ab)+){2}", "ab", res)
   call runner_postfix("a?b+|c*d", "", res)

   
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   ! call runner_postfix(, , res)
   
   if (res) then
      print *, "=== AST CASE 2 END ==="
      stop
   else
      error stop
   end if

end program main