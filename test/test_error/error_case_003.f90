program error_case_003
   use :: iso_fortran_env
   use :: forgex_test_m
   use :: forgex_error_m
   implicit none

   logical :: res = .true.

   ! Invalid characters and too short digits.
   
   print *, "=== ERROR MESSAGE CASE 3 BEGIN ==="
!=====================================================================!


   call runner_error("\x63", '', SYNTAX_VALID, res)
   call runner_error("\x{63}", '', SYNTAX_VALID, res)
   call runner_error("[\x63]", '', SYNTAX_VALID, res)

   call runner_error("[\x63-\xgf]", '', SYNTAX_ERR_INVALID_HEXADECIMAL, res)

   call runner_error("\xgg", '', SYNTAX_ERR_INVALID_HEXADECIMAL, res)
   call runner_error("\x{gg}", '', SYNTAX_ERR_INVALID_HEXADECIMAL, res)
   call runner_error("[\x{gg}]", '', SYNTAX_ERR_INVALID_HEXADECIMAL, res)
   call runner_error("\x{gg}", '', SYNTAX_ERR_INVALID_HEXADECIMAL, res)
   
   call runner_error("[\xg]", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)
   call runner_error("\xg", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)
   call runner_error("\x{g}", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)

   call runner_error("[\xa]", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)
   call runner_error("\xa", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)
   call runner_error("\x{a}", '', SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH, res)

!=====================================================================!
   if (res) then
      write(error_unit, *) "=== ERROR MESSAGE CASE 2 END ==="
      stop
   else
      error stop
   end if

end program error_case_003