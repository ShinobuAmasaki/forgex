!> These test cases cover valid regex patterns.
program main
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   print *, "=== PATTERN VALIDATE CASE 1 BEGIN ==="
!=====================================================================!

   call runner_validate("[a]{3,4}", .true., res)
   call runner_validate("(b+)a*(ab){3,4}", .true., res)
   call runner_validate("(ab|ac|a+){3,4}", .true., res)
   call runner_validate("(ab|ac){3,4}", .true., res)
   call runner_validate("(ab){3,4}", .true., res)
   call runner_validate("c{5,10}", .true., res)
   call runner_validate("c{4,10}", .true., res)
   call runner_validate("c{4,7}", .true., res)
   call runner_validate("c{2,3}", .true., res)
   call runner_validate("c{1,3}", .true., res)
   call runner_validate("c{0,3}", .true., res)

   call runner_validate("(aa|ab|ac)", .true., res)
   call runner_validate("(ああ|あい|あう)", .true., res)
   call runner_validate("(あああ|ああい|ああう)", .true., res)
   call runner_validate("あ", .true., res)
   call runner_validate("a{2}b", .true., res)
   call runner_validate("a.{1,5}g", .true., res)
   call runner_validate("(ab|aa){3}b", .true., res)

   call runner_validate("z(ab|ac|a+){3,4}", .true., res)
   call runner_validate("z(ab|aa)b", .true., res)
   call runner_validate("zx{3}(ab|aa)b", .true., res)
   call runner_validate("zx{3}(ab|aa){2}b", .true., res)
   call runner_validate("(zx{3}(ab|aa)b)+", .true., res)
   call runner_validate("(zx{3}(ab|aa)b)|(zx)", .true., res)
   call runner_validate("(zx{3}(ab|aa)b)|(z)", .true., res)
   call runner_validate("((zx{3}(ab|aa)b)|(zx))*", .true., res)


   call runner_validate("abc", .true., res)
   call runner_validate("ab*c", .true., res)
   call runner_validate("xy+z", .true., res) ! cannot get yz
   call runner_validate("(abc|def)g", .true., res)
   call runner_validate("a(bc|de)f", .true., res)
   call runner_validate("(xy|z)+a", .true., res)
   call runner_validate("ab(cd|ef)g", .true., res)
   call runner_validate("x(yz|ab)+c", .true., res)
   call runner_validate("(a|b)*cde", .true., res)
   call runner_validate("a(bc)*d", .true., res)
   call runner_validate("(ab|cd)+e", .true., res)
   call runner_validate("xy(z|w)+", .true., res)
   call runner_validate("ab{2,3}c", .true., res)
   call runner_validate("a(bc|de){2,3}f", .true., res)
   call runner_validate("xy{1,2}z", .true., res) ! cannot get yz
   call runner_validate("a[bc]d", .true., res)
   call runner_validate("[a-z]x", .true., res)
   call runner_validate("abc[def]+g", .true., res)
   call runner_validate("a(b|c)*d", .true., res)
   call runner_validate("x(yz|ab)*c", .true., res)
   call runner_validate("(ab|cd)+ef", .true., res)
   call runner_validate("(ab|cd)(ef|gh)i", .true., res)
   call runner_validate("a(b|c)d", .true., res)
   call runner_validate("(xy|z)+a", .true., res)
   call runner_validate("(ab|cd)efg", .true., res)
   call runner_validate("x(yz|ab)cd", .true., res)
   call runner_validate("a(bc|de)(fg)+h", .true., res) ! cannot get fgh

   call runner_validate("[a]{3,4}", .true., res)
   call runner_validate("(a|b)+", .true., res)
   call runner_validate("(b+)a*(ab){3,4}", .true., res)
   call runner_validate("(ab|ac|a+){3,4}", .true., res)
   call runner_validate("(ab|ac){3,4}", .true., res)
   call runner_validate("(ab|ac){3,4}z", .true., res)
   call runner_validate("(ab){3,4}", .true., res)
   call runner_validate("c{5,10}", .true., res)
   call runner_validate("c{4,10}", .true., res)
   call runner_validate("c{4,7}", .true., res)
   call runner_validate("c{2,3}", .true., res)
   call runner_validate("c{1,3}", .true., res)
   call runner_validate("c{0,3}", .true., res)

   call runner_validate("(aa|ab|ac)", .true., res)
   call runner_validate("a.{1,5}g", .true., res)
   call runner_validate("(ab|aa){3}b", .true., res)
   call runner_validate("(aa|ba){3}b", .true., res)
   call runner_validate("a(ac|bc|cc){3}", .true., res)
   call runner_validate("a(bbc|aac|abc)*", .true., res)
   call runner_validate("a(bbc|aac|abc)*b{3}", .true., res)
   call runner_validate("(ad{2,3}|cd{1,2}){3}", .true., res)
   call runner_validate("(ab?|cd?){2,3}", .true., res)
   call runner_validate("(xyz|ab|abc|ac){2,4}", .true., res)
   call runner_validate("\d{3,5}a\sb", .true., res)
   call runner_validate("((ab)?c?){2,3}", .true., res)
   call runner_validate("a?b+|c*d", .true., res)
   call runner_validate("([a-z]*g+)n?", .true., res)
   call runner_validate("[a-z]+\.(co|ne)\.jp", .true., res)
   call runner_validate("(a|b(c|d(e|f)))", .true., res)
   call runner_validate("((ab|bc)*|(de|ef)+)+", .true., res)
   call runner_validate("((ab|bb)|(db|eb))*", .true., res)
   call runner_validate("([ab]*a[ab]{20}b)((ab|bb|cb))", .true., res)
   call runner_validate("((a|b)+|(c|b)+)+", .true., res)
   call runner_validate("((a|b)+)?", .true., res)
   call runner_validate("((ac|bc)*|(dc|ec)+)+", .true., res)
   call runner_validate("([a-z]){2}", .true., res)

   call runner_validate("あ", .true., res)
   call runner_validate("(ああう|あいう|あうう)", .true., res)
   call runner_validate("(あああ|いああ|うああ)", .true., res)
   call runner_validate("(たちつ)+", .true., res)
   call runner_validate("(たちつ)*てと", .true., res)
   call runner_validate("([ぁ-ん]+)かき", .true., res)
   call runner_validate("さし([ぁ-ん]+)かき", .true., res)

   call runner_validate("(ab|bb|cb)+", .true., res)
   call runner_validate("((ab)+){2}", .true., res) !! abab is expected.
   call runner_validate("((ab|bb)|(db|eb))+", .true., res)
   call runner_validate("((ac|bc)+|(dc|ec)+)+", .true., res)
   call runner_validate("((ab)?c+){2,3}", .true., res)
   call runner_validate("((a|b)+c)+", .true., res)
   call runner_validate("a((ac|bc)+){3}", .true., res)


!=====================================================================!
   if (res) then
      print *, "=== PATTRERN VALIDATE CASE 1 END ==="
      stop
   else
      error stop
   end if

end program main
   