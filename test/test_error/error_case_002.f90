program error_case_001
   use :: iso_fortran_env
   use :: forgex_test_m
   use :: forgex_error_m
   implicit none

   logical :: res = .true.

   ! For handling errors of the special tokens.
   ! Pointed out in issue#6 on GitHub.

   print *, "=== ERROR MESSAGE CASE 2 BEGIN ==="
!=====================================================================!

   call runner_error('|', '', SYNTAX_VALID, res)
   call runner_error(')', '', SYNTAX_ERR_PARENTHESIS_UNEXPECTED, res)
   call runner_error('*', '', SYNTAX_ERR_STAR_INCOMPLETE, res)
   call runner_error('+', '', SYNTAX_ERR_PLUS_INCOMPLETE, res)
   call runner_error('?', '', SYNTAX_ERR_QUESTION_INCOMPLETE, res)
   call runner_error('\', '', SYNTAX_ERR_ESCAPED_SYMBOL_MISSING, res)
   call runner_error('{', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('}', '', SYNTAX_VALID, res)
   call runner_error('$', '', SYNTAX_VALID, res)
   call runner_error('^', '', SYNTAX_VALID, res)
   call runner_error('.', '', SYNTAX_VALID, res)
   call runner_error('-', '', SYNTAX_VALID, res)

   call runner_error('||', '', SYNTAX_VALID, res)
   call runner_error('))', '', SYNTAX_ERR_PARENTHESIS_UNEXPECTED, res)
   call runner_error('**', '', SYNTAX_ERR_STAR_INCOMPLETE, res)
   call runner_error('++', '', SYNTAX_ERR_PLUS_INCOMPLETE, res)
   call runner_error('??', '', SYNTAX_ERR_QUESTION_INCOMPLETE, res)
   call runner_error('\\', '', SYNTAX_VALID, res)
   call runner_error('{{', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('}}', '', SYNTAX_VALID, res)
   call runner_error('$$', '', SYNTAX_VALID, res)
   call runner_error('^^', '', SYNTAX_VALID, res)
   call runner_error('..', '', SYNTAX_VALID, res)
   call runner_error('--', '', SYNTAX_VALID, res)

   call runner_error('(|)', '', SYNTAX_VALID, res)
   call runner_error('())', '', SYNTAX_ERR_PARENTHESIS_UNEXPECTED, res)
   call runner_error('(*)', '', SYNTAX_ERR_STAR_INCOMPLETE, res)
   call runner_error('(+)', '', SYNTAX_ERR_PLUS_INCOMPLETE, res)
   call runner_error('(?)', '', SYNTAX_ERR_QUESTION_INCOMPLETE, res)
   call runner_error('(\)', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('({)', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('(})', '', SYNTAX_VALID, res)
   call runner_error('($)', '', SYNTAX_VALID, res)
   call runner_error('(^)', '', SYNTAX_VALID, res)
   call runner_error('(.)', '', SYNTAX_VALID, res)
   call runner_error('(-)', '', SYNTAX_VALID, res)

   call runner_error('(||', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('())', '', SYNTAX_ERR_PARENTHESIS_UNEXPECTED, res)
   call runner_error('(**', '', SYNTAX_ERR_STAR_INCOMPLETE, res)
   call runner_error('(++', '', SYNTAX_ERR_PLUS_INCOMPLETE, res)
   call runner_error('(??', '', SYNTAX_ERR_QUESTION_INCOMPLETE, res)
   call runner_error('(\\', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('({{', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('(}}', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('($$', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('(^^', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('(..', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)
   call runner_error('(--', '', SYNTAX_ERR_PARENTHESIS_MISSING, res)

   call runner_error('(||)', '', SYNTAX_VALID, res)
   call runner_error('(\\)', '', SYNTAX_VALID, res)
   call runner_error('(}})', '', SYNTAX_VALID, res)
   call runner_error('($$)', '', SYNTAX_VALID, res)
   call runner_error('(^^)', '', SYNTAX_VALID, res)
   call runner_error('(..)', '', SYNTAX_VALID, res)
   call runner_error('(--)', '', SYNTAX_VALID, res)

   call runner_error('[|]', '', SYNTAX_VALID, res)
   call runner_error('[)]', '', SYNTAX_VALID, res)
   call runner_error('[*]', '', SYNTAX_VALID, res)
   call runner_error('[+]', '', SYNTAX_VALID, res)
   call runner_error('[?]', '', SYNTAX_VALID, res)
   call runner_error('[\]', '', SYNTAX_ERR_BRACKET_MISSING, res)
   call runner_error('[{]', '', SYNTAX_VALID, res)
   call runner_error('[}]', '', SYNTAX_VALID, res)
   call runner_error('[$]', '', SYNTAX_VALID, res)
   call runner_error('[^]', '', SYNTAX_ERR_EMPTY_CHARACTER_CLASS, res)
   call runner_error('[.]', '', SYNTAX_VALID, res)
   call runner_error('[-]', '', SYNTAX_VALID, res)

   call runner_error('{|}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{)}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{*}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{+}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{?}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{\}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{{}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{}}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{$}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{^}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{.}', '', SYNTAX_ERR_INVALID_TIMES, res)
   call runner_error('{-}', '', SYNTAX_ERR_INVALID_TIMES, res)

!=====================================================================!
   if (res) then
      write(error_unit, *) "=== ERROR MESSAGE CASE 2 END ==="
      stop
   else
      error stop
   end if
   
end program error_case_001