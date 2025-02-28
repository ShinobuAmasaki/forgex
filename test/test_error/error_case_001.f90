program error_case_001
   use :: forgex_test_m
   use :: forgex_error_m
   implicit none

   logical :: res = .true.

   print *, "=== ERROR MESSAGE CASE 1 BEGIN ==="
!=====================================================================!

   ! Syntax errors
   call runner_error("abc\[]", "", SYNTAX_ERR_BRACKET_UNEXPECTED ,res)
   call runner_error("abc(", "", SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error("abc[333", "", SYNTAX_ERR_BRACKET_MISSING, res)
   call runner_error("abc]333", "", SYNTAX_ERR_BRACKET_UNEXPECTED, res)
   call runner_error("[", "", SYNTAX_ERR_BRACKET_MISSING, res)
   call runner_error("]", "", SYNTAX_ERR_BRACKET_UNEXPECTED, res)
   call runner_error("(", "", SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error(")", "", SYNTAX_ERR_PARENTHESIS_UNEXPECTED, res)
   call runner_error("{", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("}", "", SYNTAX_VALID, res)
   call runner_error("{1,2}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{2,1}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{0}", "", SYNTAX_VALID, res)
   call runner_error("a{0,a}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{-1,2}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{0,-2}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{-9999,10}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{0,-9999}", "", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("}?", "", SYNTAX_VALID, res)
   call runner_error("{0?","", SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error("a{}", "",SYNTAX_ERR_INVALID_TIMES, res)
   ! call runner_error("", res)
   ! call runner_error("", res)
   ! call runner_error("", res)

!=====================================================================!
   if (res) then
      print *, "=== ERROR MESSAGE CASE 1 END ==="
      stop
   else
      error stop
   end if
   
end program error_case_001