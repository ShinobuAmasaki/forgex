program test_005
   use :: forgex_test_m
   implicit none

   logical :: res = .true.
   character(10000) :: text
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

   pattern = "(([a-z]){1,5}){30}"
   text_a = "abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text_a = text_a//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text_a = text_a//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text_a = text_a//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text_a = text_a//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text_a = text_a//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   call runner_match(pattern, text_a, .true., res)

   pattern = "((a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z){1,5}){40}"
   text = "abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   text = trim(text)//"abcde"//"fghij"//"klmno"//"pqrst"//"uvwxy"
   call runner_match(pattern, trim(text), .true., res)   ! gfortran and ifx pass this test.



   ! Testing for discontinuous segment patterns
   pattern = "(a|c|e|g|i|k|m|o|q|s|u|w|y|1|3|5|7|9|A|C|E|G|I|K|M|O|Q|S|U|W|あ|う|お|き|け|さ|す|そ|ち|て)*"
   call runner_match(pattern, 'aCEksuW', .true., res)

   pattern = "a(a|b){200}"
   text = "ababababababababababababababababababababababababab" ! (ab) x25
   text = trim(text)//"ababababababababababababababababababababababababab" !x50
   text = trim(text)//"ababababababababababababababababababababababababab" !x75
   text = trim(text)//"ababababababababababababababababababababababababab" !x100
   text = "a"//trim(text)
   call runner_match(pattern, trim(text), .true., res)

   pattern = ".*a(a|b){500}c{20}"
   text =             "ababababababababababababababababababababababababab" ! (ab) x25
   text = trim(text)//"ababababababababababababababababababababababababab" !      x50
   text = trim(text)//"ababababababababababababababababababababababababab" !      x75
   text = trim(text)//"ababababababababababababababababababababababababab" !     x100
   text = trim(text)//"ababababababababababababababababababababababababab" !     x125
   text = trim(text)//"ababababababababababababababababababababababababab" !     x150
   text = trim(text)//"ababababababababababababababababababababababababab" !     x175
   text = trim(text)//"ababababababababababababababababababababababababab" !     x200
   text = trim(text)//"ababababababababababababababababababababababababab" !     x225
   text = trim(text)//"ababababababababababababababababababababababababab" !     x250
   text = "akkkkkkkksscga"//trim(text)
   text = trim(text)//"cccccccccccccccccccc"
   call runner_match(pattern, trim(text), .true., res)

   pattern = ".*a(a|b){500}c{20}"
   text =             "ababababababababababababababababababababababababab" ! (ab) x25
   text = trim(text)//"ababababababababababababababababababababababababab" !      x50
   text = trim(text)//"ababababababababababababababababababababababababab" !      x75
   text = trim(text)//"ababababababababababababababababababababababababab" !     x100
   text = trim(text)//"ababababababababababababababababababababababababab" !     x125
   text = trim(text)//"ababababababababababababababababababababababababab" !     x150
   text = trim(text)//"ababababababababababababababababababababababababab" !     x175
   text = trim(text)//"ababababababababababababababababababababababababab" !     x200
   text = trim(text)//"ababababababababababababababababababababababababab" !     x225
   text = trim(text)//"ababababababababababababababababababababababababab" !     x250
   text = "akkkkkkkksscga"//trim(text)
   text = trim(text)//"ccccccccccccccccccc"
   call runner_match(pattern, trim(text), .false., res)

   call runner_match("[ab]*a[ab]{20}", "abbbbbbbbbbbbbbbbbbbb", .true., res)

   call runner_in("[ab]*a[ab]{20}", "cccbbbbbabbbbbbbbbbbbbbbbbbbbccc", .true., res)

   if (res) then
      print *, "=== TEST CASE 5 END ==="
      stop
   else
      error stop
   end if
end program test_005