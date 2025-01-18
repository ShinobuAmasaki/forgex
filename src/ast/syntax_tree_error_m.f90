! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_syntax_tree_error_m module is a part of Forgex.
!
module forgex_syntax_tree_error_m
   implicit none
   
   enum, bind(c)
      enumerator :: SYNTAX_VALID
      enumerator :: SYNTAX_ERR
      enumerator :: SYNTAX_ERR_PARENTHESIS_MISSING
      enumerator :: SYNTAX_ERR_PARENTHESIS_UNEXPECTED
      enumerator :: SYNTAX_ERR_BRACKET_MISSING
      enumerator :: SYNTAX_ERR_BRACKET_UNEXPECTED
      enumerator :: SYNTAX_ERR_CURLYBRACE_MISSING
      enumerator :: SYNTAX_ERR_CURLYBRACE_UNEXPECTED
   end enum

   character(*), parameter :: err_is_nothing    = "Given pattern is valid."
   character(*), parameter :: err_generic       = "Pattern includes some syntax error."
   character(*), parameter :: err_r_parenthesis_missing    = "Closing parenthesis is expected."
   character(*), parameter :: err_r_parenthesis_unexpected = "Unexpected closing parenthesis error."
   character(*), parameter :: err_r_bracket_missing        = "Closing square bracket is expected."
   character(*), parameter :: err_r_bracket_unexpected     = "Unexpected closing square bracket error."
   character(*), parameter :: err_r_curlybrace_missing     = "Closing right curlybrace is expected."
   character(*), parameter :: err_r_curlybrace_unexpected  = "Unexpected closing right curlybrace error."

contains

   pure function get_error_message(code) result(msg)
      implicit none
      integer, intent(in)       :: code
      character(:), allocatable :: msg

      select case (code)
      case (SYNTAX_ERR)
         msg = err_generic
      case (SYNTAX_ERR_PARENTHESIS_MISSING)
         msg = err_r_parenthesis_missing
      case (SYNTAX_ERR_PARENTHESIS_UNEXPECTED)
         msg = err_r_parenthesis_unexpected
      case (SYNTAX_ERR_BRACKET_MISSING)
         msg = err_r_bracket_missing
      case (SYNTAX_ERR_BRACKET_UNEXPECTED)
         msg = err_r_bracket_unexpected
      case (SYNTAX_ERR_CURLYBRACE_MISSING)
         msg = err_r_curlybrace_missing
      case (SYNTAX_ERR_CURLYBRACE_UNEXPECTED)
         msg = err_r_curlybrace_unexpected
      case default
         msg = err_is_nothing
      end select

   end function get_error_message


end module forgex_syntax_tree_error_m