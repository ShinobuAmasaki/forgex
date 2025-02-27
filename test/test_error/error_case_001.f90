program error_case_001
   use :: forgex_test_m
   use :: forgex_error_m
   implicit none

   logical :: res = .true.

   print *, "=== ERROR MESSAGE CASE 1 BEGIN ==="
!=====================================================================!

   call runner_error("abc\[]", "", SYNTAX_ERR_BRACKET_UNEXPECTED ,res)
   call runner_error("abc(", "", SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error("abc[333", "", SYNTAX_ERR_BRACKET_MISSING, res)
   call runner_error("abc]333", "", SYNTAX_ERR_BRACKET_UNEXPECTED, res)
   call runner_error("[", "", SYNTAX_ERR_BRACKET_MISSING, res)
   call runner_error("]", "", SYNTAX_ERR_BRACKET_UNEXPECTED, res)

!=====================================================================!
   if (res) then
      print *, "=== ERROR MESSAGE CASE 1 END ==="
      stop
   else
      error stop
   end if
   
end program error_case_001