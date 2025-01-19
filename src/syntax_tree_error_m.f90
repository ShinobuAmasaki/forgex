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
      enumerator :: SYNTAX_ERR_INVALID_RANGE
   end enum

   character(*), parameter :: err_is_nothing    = "Given pattern is valid."
   character(*), parameter :: err_generic                  = "ERROR: Pattern includes some syntax error."
   character(*), parameter :: err_r_parenthesis_missing    = "ERROR: Closing parenthesis is expected."
   character(*), parameter :: err_r_parenthesis_unexpected = "ERROR: Unexpected closing parenthesis error."
   character(*), parameter :: err_r_bracket_missing        = "ERROR: Closing square bracket is expected."
   character(*), parameter :: err_r_bracket_unexpected     = "ERROR: Unexpected closing square bracket error."
   character(*), parameter :: err_r_curlybrace_missing     = "ERROR: Closing right curlybrace is expected."
   character(*), parameter :: err_r_curlybrace_unexpected  = "ERROR: Unexpected closing right curlybrace error."
   character(*), parameter :: err_invalid_range            = "ERROR: Given range is invalid."

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

      case (SYNTAX_ERR_INVALID_RANGE)
         msg = err_invalid_range

      case default
         msg = err_is_nothing
      end select

   end function get_error_message


end module forgex_syntax_tree_error_m