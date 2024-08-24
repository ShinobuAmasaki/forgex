program test_case_007
   use ::forgex_test_m
   implicit none

   logical :: res = .true.

   ! Nested cases

   print *, "=== TEST CASE 7 BEGIN ==="
   call runner_match("(a|b)*", 'ababab', .true., res)
   call runner_match("(a|b)+", 'ababab', .true., res)
   call runner_match("(a|b)?", 'a', .true., res)

   call runner_match("((a|b)*)*", 'ababab', .true., res)
   call runner_match("((a|b)*){0,}", 'ababab', .true., res)
   call runner_match("((a|b)*)?", 'ababab', .true., res)
   call runner_match("((a|b)*)+", 'ababab', .true., res)

   call runner_match("(a*)*", "", .true., res)
   call runner_match("(a*)*", "a", .true., res)
   call runner_match("(a*)*", "aaa", .true., res)
   call runner_match("(a*)*", "aaaaa", .true., res)

   call runner_match("a?b+|c*d", "bbbb", .true., res)
   call runner_match("a?b+|c*d", "a", .false., res)
   call runner_match("a?b+|c*d", "b", .true., res)
   call runner_match("a?b+|c*d", "cd", .true., res)
   call runner_match("a?b+|c*d", "d", .true., res)
   call runner_match("a?b+|c*d", "a", .false., res)

   call runner_match("(a|b(c|d))*", "bc", .true., res)
   call runner_match("(a|b(c|d(e|f)))", "a", .true., res)

   call runner_match("(c|d(e|f)g)*", "cdfgcc", .true., res)
   call runner_match("(c|d(e|f)g)*", "cdfgcdeg", .true., res)

   call runner_match("(a|b(c|d(e|f)g)h|i)", "a", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)", "bdegh", .true., res)

   call runner_match("(a|b(c|d(e|f)g)h|i)*", "a", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "a", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "i", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bdegh", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bdeghiii", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "iii", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bdegh", .true., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "ai", .true., res)

   call runner_match("(a|b(c|d(e|f)g)h|i)*", "abb", .false., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bcfg", .false., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "j", .false., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bddghi", .false., res)
   call runner_match("(a|b(c|d(e|f)g)h|i)*", "bdfhi", .false., res)

   call runner_match("(a|b(c|dg)h|i)*", "bch", .true., res)

   call runner_match("((ab|bc)*|(de|ef)+)+", "ab", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "bc", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "abab", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "de", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "ef", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "deef", .true., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "abdebc", .true., res)

   call runner_match("((ab|bc)*|(de|ef)+)+", "a", .false., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "b", .false., res)
   call runner_match("((ab|bc)*|(de|ef)+)+", "abed", .false., res)
   call runner_match("(ab){3}b", "abababb", .true., res)



   call runner_in("(a|b)*", 'ababab', .true., res)
   call runner_in("(a|b)+", 'ababab', .true., res)
   call runner_in("(a|b)?", 'a', .true., res)
   call runner_in("((a|b)*)*", 'ababab', .true., res)
   call runner_in("((a|b)*){0,}", 'ababab', .true., res)
   call runner_in("((a|b)*)?", 'ababab', .true., res)
   call runner_in("((a|b)*)+", 'ababab', .true., res)
   call runner_in("(a*)*", "", .true., res) !!
   call runner_in("(a*)*", "a", .true., res)
   call runner_in("(a*)*", "aaa", .true., res)
   call runner_in("(a*)*", "aaaaa", .true., res)

   call runner_in("a?b+|c*d", "bbbb", .true., res)
   call runner_in("a?b+|c*d", "a", .false., res)
   call runner_in("a?b+|c*d", "b", .true., res)
   call runner_in("a?b+|c*d", "cd", .true., res)
   call runner_in("a?b+|c*d", "d", .true., res)
   call runner_in("a?b+|c*d", "a", .false., res)

   call runner_in("(a|b(c|d))*", "bc", .true., res)
   call runner_in("(a|b(c|d(e|f)))", "a", .true., res)

   call runner_in("(c|d(e|f)g)*", "cdfgcc", .true., res)
   call runner_in("(c|d(e|f)g)*", "cdfgcdeg", .true., res)

   call runner_in("(a|b(c|d(e|f)g)h|i)", "a", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)", "bdegh", .true., res)

   call runner_in("(a|b(c|d(e|f)g)h|i)*", "a", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "", .true., res) !!
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "a", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "i", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bdegh", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bdeghiii", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "iii", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bdegh", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "ai", .true., res)

   call runner_in("(a|b(c|d(e|f)g)h|i)*", "abb", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bcfg", .false., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "j", .false., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bddghi", .true., res)
   call runner_in("(a|b(c|d(e|f)g)h|i)*", "bdfhi", .true., res)

   call runner_in("(a|b(c|dg)h|i)*", "bch", .true., res)

   call runner_in("((ab|bc)*|(de|ef)+)+", "ab", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "bc", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "abab", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "de", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "ef", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "deef", .true., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "abdebc", .true., res)

   call runner_in("((ab|bc)*|(de|ef)+)+", "a", .false., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "b", .false., res)
   call runner_in("((ab|bc)*|(de|ef)+)+", "abed", .true., res)
   call runner_in("(ab){3}b", "abababb", .true., res)
   call runner_in("a?b+|c*d", "bbd", .true., res)

   call runner_in("(^a)", "alpha", .true., res)
   call runner_in("(^a)|.*a\s*$", "alpha", .true., res)
   call runner_in("(a\s*$)", "alpha", .true., res)
   call runner_in("(^a)|(a$)", "alpha", .true., res)


   if (res) then
      print *, "=== TEST CASE 7 END ==="
      stop
   else
      error stop "There are cases where the match fails."
   end if


end program test_case_007