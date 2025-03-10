program main
   use :: iso_fortran_env
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
   call runner_prefix("zx{3}(ab|aa)b", "zxxxa", res)
   call runner_prefix("zx{3}(ab|aa){2}b", "zxxxa", res)
   call runner_prefix("(zx{3}(ab|aa)b)+", "zxxxa", res)
   call runner_prefix("(zx{3}(ab|aa)b)|(zx)", "zx", res)
   call runner_prefix("(zx{3}(ab|aa)b)|(z)", "z", res)
   call runner_prefix("((zx{3}(ab|aa)b)|(zx))*", "", res)


   call runner_prefix('\t', char(9), res)
   call runner_prefix('\n', '', res)
   call runner_prefix('\r', char(13), res)
   call runner_prefix('\s', '', res)
   call runner_prefix('\S', '', res)
   call runner_prefix('\w', '', res)
   call runner_prefix('\W', '', res)
   call runner_prefix('\d', '', res)
   call runner_prefix('\D', '', res)
   call runner_prefix('\d', '', res)

   call runner_prefix('a[b-d]*x', 'a', res)
   call runner_prefix('(a[b-d]+)x*', 'a', res)
   call runner_prefix('(a[b-d]+)x*', 'a', res)
   call runner_prefix('\dabc', '', res)

   call runner_prefix('x+', 'x', res)
   call runner_prefix('a[b-d]x', 'a', res)
   call runner_prefix('a[b-d]*x', 'a', res)
   call runner_prefix('a([b-d]*)x+', 'a', res)
   call runner_prefix('(a[b-d]+)x*', 'a', res)
   call runner_prefix('(a[b-d]+)x*', 'a', res)

   call runner_prefix('v[ab]w[cd]x[ef]y[gh]z', 'v', res)
   call runner_prefix('\dabc', '', res)
   call runner_prefix('abc\d+', 'abc', res)
   call runner_prefix('\d+abc\d*', '', res)
   call runner_prefix('a(ac|bc|cc){3}', 'a', res)
   call runner_prefix('[ab]*(ab|bb|cb)', '', res)
   call runner_prefix('(z[ab]d)', 'z', res)
   call runner_prefix('([ab]*)((ab|bb|cb))', '', res)
   call runner_prefix('(z[ab]*ab)((ab|bb|cb))', 'z', res)
   call runner_prefix("(z[ab]*a[ab]{20}b)((ab|bb|cb))", 'z', res)
   call runner_prefix('x[a-z]+y[a-z]+z', 'x', res)

   call runner_prefix('(b+)(ab){3,4}', 'b', res)
   call runner_prefix('(b+)a*(ab){3,4}', 'b', res)
   call runner_prefix('x[a-z]+y[a-z]+z', 'x', res)
   call runner_prefix('(b+)(ab){3,4}','b', res)
   call runner_prefix('(b+)a*(ab){3,4}','b', res)
   call runner_prefix('[a-z]+ab','', res)
   call runner_prefix('(a[a-z]+)([A-Z]b)','a', res)

   call runner_prefix('(w[a-z]*x)(y[A-Z]*z)','w', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   ! call runner_prefix('','', res)
   
   if (res) then
      write(error_unit, *) "=== AST CASE 1 END ==="
      stop
   else
      error stop
   end if

end program main