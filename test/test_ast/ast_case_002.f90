program main
   use :: forgex_test_m
   implicit none
   
   logical :: res = .true.

   print *, "=== AST CASE 2 BEGIN ==="

   call runner_suffix("[a]{3,4}", "aaa", res)
   call runner_suffix("(a|b)+", "", res)
   call runner_suffix("(b+)a*(ab){3,4}", "ababab", res)
   call runner_suffix("(ab|ac|a+){3,4}", "", res)
   call runner_suffix("(ab|ac){3,4}", "", res)
   call runner_suffix("(ab|ac){3,4}z", "z", res)
   call runner_suffix("(ab){3,4}", "ababab", res)
   call runner_suffix("c{5,10}", "ccccc", res)
   call runner_suffix("c{4,10}", "cccc", res)
   call runner_suffix("c{4,7}", "cccc", res)
   call runner_suffix("c{2,3}", "cc", res)
   call runner_suffix("c{1,3}", "c", res)
   call runner_suffix("c{0,3}", "", res)

   call runner_suffix("(aa|ab|ac)", "", res)
   call runner_suffix("(ああう|あいう|あうう)", "う", res)
   call runner_suffix("(あああ|いああ|うああ)", "ああ", res)
   call runner_suffix("あ", "あ", res)
   call runner_suffix("a.{1,5}g", "g", res)
   call runner_suffix("(ab|aa){3}b", "b", res)
   call runner_suffix("(aa|ba){3}b", "ab", res)
   call runner_suffix("a(ac|bc|cc){3}", "c", res)
   call runner_suffix("a(bbc|aac|abc)*", "", res)
   call runner_suffix("a(bbc|aac|abc)*b{3}", "bbb", res)
   call runner_suffix("(ad{2,3}|cd{1,2}){3}", "d", res)
   call runner_suffix("(ab?|cd?){2,3}", "", res)
   call runner_suffix("(xyz|ab|abc|ac){2,4}", "", res)
   call runner_suffix("\d{3,5}a\sb", "b", res)
   call runner_suffix("((ab)?c?){2,3}", "", res)
   call runner_suffix("a?b+|c*d", "", res)
   call runner_suffix("([a-z]*g+)n?", "", res)
   call runner_suffix("[a-z]+\.(co|ne)\.jp", ".jp", res)
   call runner_suffix("(a|b(c|d(e|f)))", "", res)
   call runner_suffix("((ab|bc)*|(de|ef)+)+", "", res)
   call runner_suffix("((ab|bb)|(db|eb))*", "", res)
   call runner_suffix("([ab]*a[ab]{20}b)((ab|bb|cb))", "b", res)
   call runner_suffix("((a|b)+|(c|b)+)+", "", res)
   call runner_suffix("((a|b)+)?", "", res)
   call runner_suffix("((ac|bc)*|(dc|ec)+)+", "", res)
   call runner_suffix("([a-z]){2}", "", res)


   !! If the + operator is not processed on op_closure handling in get_suffix_literal_internal,
   !! no suffix is ​​obtained.
   ! call runner_suffix("(ab|bb|cb)+", "", res)
   ! call runner_suffix("((ab)+){2}", "", res)
   ! call runner_suffix("((ab|bb)|(db|eb))+", "", res)
   ! call runner_suffix("((ac|bc)+|(dc|ec)+)+", "", res)
   ! call runner_suffix("((ab)?c+){2,3}", "", res)
   ! call runner_suffix("((a|b)+c)+", "", res)
   
   !! The implementation of op_closure handling in get_suffix_literal_internal has weak test coverage.
   call runner_suffix("(ab|bb|cb)+", "b", res)
   call runner_suffix("((ab)+){2}", "ab", res) !! abab is expected.
   call runner_suffix("((ab|bb)|(db|eb))+", "b", res)
   call runner_suffix("((ac|bc)+|(dc|ec)+)+", "c", res)
   call runner_suffix("((ab)?c+){2,3}", "c", res)
   call runner_suffix("((a|b)+c)+", "c", res)
   call runner_suffix("a((ac|bc)+){3}", "c", res)

   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   ! call runner_suffix(, , res)
   
   if (res) then
      print *, "=== AST CASE 2 END ==="
      stop
   else
      error stop
   end if

end program main