program test_005
   use :: forgex_test_m
   implicit none

   logical :: res = .true.
   character(1000) :: text
   character(:), allocatable :: text_a
   character(:), allocatable ::  pattern

   ! Stress-test cases

   print *, "=== TEST CASE 5 BEGIN ==="

   call runner_match("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .true., res)

   call runner_match("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaakaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .false., res)

   call runner_in("a(a|aa)*b", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
   &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaakaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", .true., res)

   call runner_match("((a|b|c|d|e|f|g|h|i|j){1,5}){,3}", "abcdefghij", .true., res)

   call runner_match("aa*b", "cb", .false., res)
   call runner_match("a(a|aa)*b", "acb", .false., res)

   call runner_match("((a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z){1,5}){,5}", &
   "abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy", .true., res)


   pattern = "(([a-z]){1,5}){30}"
   text = "abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   call runner_match(pattern, trim(text), .true., res)   ! gfortran and ifx pass this test.


   !== both gfortran and ifx cannot pass this test ==!
   ! pattern = "(([a-z]){1,5}){30}"
   ! text_a = "abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! text_a = trim(text_a)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! text_a = trim(text_a)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! text_a = trim(text_a)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! text_a = trim(text_a)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! text_a = trim(text_a)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   ! call runner_match(pattern, text, .true., res)

   if (res) then
      print *, "=== TEST CASE 5 END ==="
      stop
   else
      error stop
   end if
end program test_005