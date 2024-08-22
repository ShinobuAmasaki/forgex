program main
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== AST CASE 1 BEGIN ==="

   call runner_prefix("[a]{3,4}", "aaa", res)
   call runner_prefix("(b+)a*(ab){3,4}", "b", res)
   call runner_prefix("(ab|ac|a+){3,4}", "a", res)
   call runner_prefix("(ab|ac){3,4}", "a", res)
   call runner_prefix("(ab){3,4}", "ababab", res)
   call runner_prefix("c{5,10}", "ccccc", res)
   call runner_prefix("c{4,10}", "cccc", res)
   call runner_prefix("c{4,7}", "cccc", res)
   call runner_prefix("c{2,3}", "cc", res)
   call runner_prefix("c{1,3}", "c", res)
   call runner_prefix("c{0,3}", "", res)

   call runner_prefix("(aa|ab|ac)", "a", res)
   call runner_prefix("(ああ|あい|あう)", "あ", res)
   call runner_prefix("(あああ|ああい|ああう)", "ああ", res)
   call runner_prefix("あ", "あ", res)
   call runner_prefix("a{2}b", "aab", res)
   call runner_prefix("a.{1,5}g", "a", res)
   call runner_prefix("(ab|aa){3}b", "a", res)

   call runner_prefix("z(ab|ac|a+){3,4}", "za", res)
   call runner_prefix("z(ab|aa)b", "za", res)

   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   ! call runner_prefix(, , res)
   
   if (res) then
      print *, "=== AST CASE 1 END ==="
      stop
   else
      error stop
   end if

end program main