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
      enumerator :: SYNTAX_ERR_PARENTHESIS
      enumerator :: SYNTAX_ERR_BRACKET
      enumerator :: SYNTAX_ERR_CURLYBRACE
   end enum

   character(*), parameter :: err_is_nothing  = "Given pattern is valid."
   character(*), parameter :: err_generic     = "Pattern includes some syntax error."
   character(*), parameter :: err_parenthesis = "Closing parenthesis is expected."
   character(*), parameter :: err_bracket     = "Closing square bracket is expected."
   character(*), parameter :: err_curlybrace  = "Closing right curlybrace is expected."

contains

   pure function get_error_message(code) result(msg)
      implicit none
      integer, intent(in)       :: code
      character(:), allocatable :: msg

      select case (code)
      case (SYNTAX_ERR)
         msg = err_generic
      case (SYNTAX_ERR_PARENTHESIS)
         msg = err_parenthesis
      case (SYNTAX_ERR_BRACKET)
         msg = err_bracket
      case (SYNTAX_ERR_CURLYBRACE)
         msg = err_curlybrace
      case default
         msg = err_is_nothing
      end select

   end function get_error_message


end module forgex_syntax_tree_error_m